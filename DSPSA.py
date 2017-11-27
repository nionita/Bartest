# -*- coding: utf-8 -*-
"""
Created on Mon Apr 24 15:59:24 2017

@author: nicu
"""

import numpy as np
import random
import math
import subprocess
import re
import os
import pickle

# DSPSA
# We have to optimize a stochastic function of n integer parameters,
# but we can only get noisy results from measurements

# Transform Elo to fraction and back
ln10p400 = -math.log(10.0) / 400.0
iln10p400 = 1. / ln10p400

def elo2frac(elo):
    return 1.0 / (1.0 + math.exp(elo * ln10p400))

def frac2elo(frac):
    return math.log(1. / frac - 1.) * iln10p400

# The optimisation: we make a simplified one first, to check the concept
# It optimizes in integers
class Optimizer:
    def __init__(self, x, steps=1000, radius=3, bandits=None):
        if not isinstance(x, tuple):
            raise(TypeError)
        self.dims = len(x)
        self.fac  = 2 ** self.dims
        self.best = x
        self.steps = steps
        self.radius = radius
        if bandits is None:
            self.bandits = self.dims + 1
        else:
            self.bandits = bandits
        self.results = dict()
        self.debug = False
        self.complete()

    def complete(self):
        self.simp = [self.best]
        self.since = 0
        while len(self.simp) < self.bandits:
            xl = list(self.best)
            i = random.randint(0, self.dims-1)
            d = random.randint(1, self.radius)
            s = random.choice([False, True])
            if s:
                xl[i] = xl[i] + d
            else:
                xl[i] = xl[i] - d
            t = tuple(xl)
            if t not in self.simp:
                self.simp.append(t)
        for x in self.simp:
            if x not in self.results:
                self.results[x] = Thompson()
                if self.debug:
                    print('add new Thomson:', x, ':', self.results[x].thid)
            if self.debug:
                print('in simp', x, ':', self.results[x].thid)

    def neighbours(self, x):
        xl = list(x)
        nbs = []
        for i in range(self.dims):
            xi = xl[i]
            xl[i] = xi + 1
            nbs.append(tuple(xl))
            xl[i] = xi - 1
            nbs.append(tuple(xl))
            xl[i] = xi
        return nbs

    def step_prep(self):
        best = None
        bestv = -math.inf
        for x in self.simp:
            v = self.results[x].simulate(self.fac)
            if self.debug:
                print('Step:', x, '-->', v)
            if best is None or v > bestv:
                #if self.debug:
                #    print('Step: new best', x)
                best = x
                bestv = v
        self.sbest = best
        return best

    def step_exec(self):
        # Now we have the best simulated result, play it in real
        w, l = play(self.sbest)
        # Now reward the played model and update the prior for his neighbours
        if w > 0 or l > 0:
            self.results[self.sbest].reward(w, l)
            for nb in self.neighbours(self.sbest):
                if nb in self.results:
                    self.results[nb].add_prior(w, l)
                else:
                    t = Thompson(prion=w, prize=l)
                    self.results[nb] = t
        t = self.results[self.sbest]
        s, _ = t.score()
        if self.debug:
            print('Step play:', self.sbest, s, 'in', t.played(), '(', t.thid, ')')
        self.newbest()
        return w, l, s

    def allbests(self):
        bests = []
        bestv = -math.inf
        for x, t in self.results.items():
            v, _ = t.score()
            if v > bestv:
                bests = [x]
                bestv = v
            elif v == bestv:
                bests.append(x)
        return bests, bestv

    def lowbests(self):
        bests = []
        bestv = -math.inf
        for x, t in self.results.items():
            _, v = t.lowscore()
            if v > bestv:
                bests = [x]
                bestv = v
            elif v == bestv:
                bests.append(x)
        return bests, bestv

    def newbest(self):
        self.since += 1
        if self.since > self.steps:
            bests, _, = self.allbests()
            #bests, _ = self.lowbests()
            self.best = random.choice(bests)
            if self.debug:
                for x in bests:
                    t = self.results[x]
                    s, _ = t.score()
                    print('best: score = %.3f, played = %d (%d)' % (s, t.played(), t.thid))
                for x in self.results:
                    t = self.results[x]
                    s, _ = t.score()
                    print('really best: score = %.3f, played = %d (%d)' % (s, t.played(), t.thid))
            self.complete()

    def rep_sc(self, x):
        t = self.results[x]
        s, e = t.score()
        print(' -', x, ': score = %.3f, elo = %.1f, played = %d (%d)' % (s, e, t.played(), t.thid))

    def report(self):
        bests, bestv = self.allbests()
        print('All bests:', bestv)
        for x in bests:
            self.rep_sc(x)
        bests, bestv = opt.lowbests()
        print('Low bests:', bestv)
        for x in bests:
            self.rep_sc(x)
        most = []
        mpld = 0
        for x in self.results:
            t = self.results[x]
            p = t.played()
            if p > mpld:
                most = [x]
                mpld = p
            elif p == mpld:
                most.append(x)
        print('Most played:', mpld)
        for x in most:
            self.rep_sc(x)
        print('Total players:', len(self.results), '(', maxi/len(self.results), ')')

    def save(self, suf):
        fn = 'optim' + suf + '.sav'
        with open(fn, 'wb') as file:
            pickle.Pickler(file, 2).dump(self)

"""
Implementation of DSPSA
"""
class DSPSA:
    def __init__(self, pnames, theta, laststep, alpha=0.501, msteps=1000, scale=None, rend=None):
        self.pnames = pnames
        self.smalla = laststep * math.pow(1.1 * msteps + 1, alpha)
        self.biga = 0.1 * msteps
        self.alpha = alpha
        self.msteps = msteps
        self.theta = np.array(theta, dtype=np.float32)
        if scale is None:
            self.scale = None
        else:
            self.scale = np.array(scale, dtype=np.float32)
        self.rend = rend

    def optimize(self, f, config):
        p = self.theta.shape[0]
        theta = self.theta
        rtheta = np.rint(theta)
        since = 0
        for k in range(self.msteps):
            if k % 1 == 0:
                print('Step:', k)
            delta = 2 * np.random.randint(0, 2, size=p) - np.ones(p, dtype=np.int)
            pi = np.floor(theta) + np.ones(p, dtype=np.float32) / 2
            tp = np.rint(pi + delta / 2)
            tm = np.rint(pi - delta / 2)
            df = f(tp, tm, config)
            gk = df / delta
            ak = self.smalla / math.pow(1 + self.biga + k, self.alpha)
            if k % 1 == 0:
                print('df:', df, 'ak:', ak)
            # Here: + because we maximize!
            theta = theta + ak * gk
            if k % 1 == 0:
                print('theta:', theta)
            ntheta = np.rint(theta)
            if np.all(ntheta == rtheta):
                since += 1
                if self.rend is not None and since >= self.rend:
                    break
            else:
                rtheta = ntheta
                since = 0
            if k % 10 == 0:
                self.report(theta)
        return rtheta

    """
    Momentum optimizer with friction
    beta1 + beta2 <= 1
    """
    def momentum(self, f, config, beta1=0.8, beta2=0.1):
        p = self.theta.shape[0]
        gm = np.zeros(p, dtype=np.float32)
        theta = self.theta
        for k in range(self.msteps):
            if k % 1 == 0:
                print('Step:', k)
            delta = 2 * np.random.randint(0, 2, size=p) - np.ones(p, dtype=np.int)
            pi = np.floor(theta) + np.ones(p, dtype=np.float32) / 2
            tp = np.rint(pi + delta / 2)
            tm = np.rint(pi - delta / 2)
            df = f(tp, tm, config)
            gk = df / delta
            gm = gm * beta1 + gk * beta2
            # We wouldn't need biga, as first steps are biased towards 0 anyway
            ak = self.smalla / math.pow(1 + self.biga + k, self.alpha)
            if k % 1 == 0:
                print('df:', df, 'ak:', ak)
            # Here: + because we maximize!
            theta = theta + ak * gm
            if k % 1 == 0:
                print('theta:', theta)
            if k % 10 == 0:
                self.report(theta)
        return np.rint(theta)

    """
    Adadelta should mantain different learning rates per dimension, but in our
    case all dimensions would have equal rates, because in every step only
    the sign is different, and we can't break the simmetry.
    Also, our gradient is just an estimate.
    To deal with these problems we mantain an average gradient and work with it
    as if it would be the current one
    """
    def adadelta(self, f, config, beta=0.9, gamma=0.9):
        eps = 1E-6
        p = self.theta.shape[0]
        gm = np.zeros(p, dtype=np.float32)
        eg2 = np.zeros(p, dtype=np.float32)
        ed2 = np.zeros(p, dtype=np.float32)
        theta = self.theta
        for k in range(self.msteps):
            print('Step:', k)
            delta = 2 * np.random.randint(0, 2, size=p) - np.ones(p, dtype=np.int)
            if self.scale is not None:
                delta = delta * self.scale
            pi = np.floor(theta) + np.ones(p, dtype=np.float32) / 2
            tp = np.rint(pi + delta / 2)
            tm = np.rint(pi - delta / 2)
            df = f(tp, tm, config)
            gk = df / delta
            gm = beta * gm + (1 - beta) * gk
            eg2 = gamma * eg2 + (1 - gamma) * gm * gm
            dtheta = np.sqrt((ed2 + eps) / (eg2 + eps)) * gm
            ed2 = gamma * ed2 + (1 - gamma) * dtheta * dtheta
            # We don't need other learning rates
            # Here: + because we maximize!
            theta = theta + dtheta
            print('df:', df, 'gm norm:', np.linalg.norm(gm), 'dt norm:', np.linalg.norm(dtheta))
            print('theta:', theta)
            if k % 10 == 0:
                self.report(theta)
        return np.rint(theta)

    def report(self, vec, title=None, file='report.txt'):
        if title is None:
            title = 'Current best:'
        if file is None:
            print(title)
            for n, v in zip(self.pnames, list(vec)):
                print(n, '=', v)
        else:
            with open(file, 'w', encoding='utf-8') as repf:
                print(title, file=repf)
                for n, v in zip(self.pnames, list(vec)):
                    print(n, '=', v, file=repf)

class Config:
    def __init__(self, selfplay='', playdir='.', ipgnfile='', depth=4, games=16, params=[]):
        self.selfplay = selfplay
        self.playdir = playdir
        self.ipgnfile = ipgnfile
        self.depth = depth
        self.games = games
        self.params = params

paramWeights = [
          #('kingSafe'      , 1, 0, 1),
          ('kingOpen'      , 5, 0, 1),
          ('kingPlaceCent' , 6, 0, 1),
          ('kingPlacePwns' , 0, 6, 1),
          ('kingPawn1'     , 8, 48, 5),
          ('kingPawn2'     , 12, 64, 5),
          ('rookHOpen'     , 160, 180, 4),
          ('rookOpen'      , 211, 186, 4),
          ('rookConn'      , 94,  53, 5),
          ('mobilityKnight', 46, 61, 1),
          ('mobilityBishop', 52, 29, 1),
          ('mobilityRook'  , 23, 25, 1),
          ('mobilityQueen' ,  4,  3, 1),
          ('centerPAtts'   , 76, 59, 3),
          ('centerNAtts'   , 44, 41, 3),
          ('centerBAtts'   , 52, 38, 3),
          ('centerRAtts'   , 10, 30, 1),
          ('centerQAtts'   ,  4, 55, 1),
          ('centerKAtts'   ,  2, 54, 1),
          #('space'         ,  1,  0, 1),
          ('adversAtts'       ,  2, 14, 1),
          ('isolPawns'     , (-37), (-108), 5),
          ('isolPassed'    , (-51), (-152), 5),
          ('backPawns'     , (-113), (-151), 5),
          ('backPOpen'     , (-23),  (-20), 5),
          ('enpHanging'    , (-21), (-34), 4),
          ('enpEnPrise'    , (-28), (-26), 4),
          ('enpAttacked'   ,  (-6), (-7), 2),
          ('wepAttacked'   , 48, 64, 4),
          ('lastLinePenalty', 107, 3, 2),
          ('bishopPair'    , 390, 320, 5),
          ('bishopPawns'   , (-22), (-58), 2),
          ('redundanceRook', (-29), (-61), 5),
          ('rookPawn'      , (-47), (-37), 3),
          ('advPawn5'      ,    4, 118, 3),
          ('advPawn6'      ,  356, 333, 5),
          ('pawnBlockP'    , (-115), (-90), 3),
          ('pawnBlockO'    ,  (-20), (-23), 3),
          ('pawnBlockA'    ,  (-13), (-70), 3),
          ('passPawnLev'   ,  0, 9, 1),
        ]

resre = re.compile(r'End result')
wdlre = re.compile('[() ,]')

# Play a match with a given number of games between theta+ and theta-
# Player 1 is theta+
# Player 2 is theta-
def play(tp, tm, config):
    os.chdir(config.playdir)
    with open('playerp.cfg', 'w', encoding='utf-8') as plf:
        for p, v in zip(config.params, tp):
            plf.write('%s=%d\n' % (p, v))
    with open('playerm.cfg', 'w', encoding='utf-8') as plf:
        for p, v in zip(config.params, tm):
            plf.write('%s=%d\n' % (p, v))
    skip = random.randint(0, 25000)
    #print('Skip = %d' % skip)
    args = [config.selfplay, '-m', config.playdir, '-a', 'playerp.cfg', '-b', 'playerm.cfg',
            '-i', config.ipgnfile, '-d', str(config.depth), '-s', str(skip), '-f', str(config.games)]
    #print('Will start:')
    #print(args)
    w = None
    # For windows: shell=True to hide the window of the child process
    with subprocess.Popen(args, bufsize=1, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                          cwd=config.playdir, universal_newlines=True, shell=True) as proc:
        for line in proc.stdout:
            #print('Got:', line)
            if resre.match(line):
                #vals = wdlre.split(line)
                #print(vals)
                _, _, _, ws, ds, ls, _ = wdlre.split(line)
                w = int(ws)
                d = int(ds)
                l = int(ls)
                #print('I found the result %d, %d, %d' % (w, d, l))
    if w == None:
        raise RuntimeError('No result from self play')
    else:
        return (w + 0.5 * d) / (w + d + l) - 0.5

def square(x):
    f = (x[0] * x[0] + 2 * x[1] * x[1])/100 + np.random.randn()
    return f

def square_diff(vp, vm, conf):
    fp = square(vp)
    fm = square(vm)
    # Here we invert to minimize
    return fm - fp

def banana(v):
    x = v[0]
    y = v[1]
    f = (1 - x) * (1 - x) + 100 * (y - x*x) * (y - x*x) + np.random.randn()
    return f

def banana_diff(vp, vm, conf):
    fp = banana(vp)
    fm = banana(vm)
    # Here we invert to minimize
    return fm - fp

if __name__ == '__main__':
    pnames = []
    pinits = []
    pscale = []
    hasScale = False
    for name, mid, end, scale in paramWeights:
        pnames.append('mid.' + name)
        pinits.append(mid)
        pscale.append(scale)
        pnames.append('end.' + name)
        pinits.append(end)
        pscale.append(scale)
        if scale != 1:
            hasScale = True

    config = Config(selfplay=r'C:\astra\SelfPlay-soku.exe',
                    playdir=r'C:\astra\play', ipgnfile=r'C:\astra\open-moves\open-moves.fen',
                    depth=4, games=2,
                    params=pnames)

    if not hasScale:
        scale = None

    opt = DSPSA(pnames, pinits, 0.03, msteps=10000, scale=scale)
    #r = opt.optimize(play, config)
    #r = opt.momentum(play, config)
    r = opt.adadelta(play, config, beta=0.98)
    #opt = DSPSA(['x', 'y'], [3, 3], 0.03, msteps=100)
    #r = opt.adadelta(banana_diff, config)
    #r = opt.adadelta(square_diff, config)
    opt.report(r, title='Optimum', file='optimum.txt')
    opt.report(r, title='Optimum', file=None)