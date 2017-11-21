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
import sys

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
    def __init__(self, pnames, theta, smalla, biga=None, alpha=0.501, gmax=None, scale=None, msteps=1000, rend=None):
        self.pnames = pnames
        self.smalla = smalla
        self.biga = biga
        self.alpha = alpha
        self.gmax = gmax
        self.msteps = msteps
        self.rend = rend
        self.theta = np.array(theta, dtype=np.float32)
        if scale is None:
            self.scale = scale
        else:
            self.scale = np.array(scale, dtype=np.float32)

    def optimize(self, f, config):
        p = self.theta.shape[0]
        theta = self.theta
        rtheta = np.rint(theta)
        since = 0
        for k in range(self.msteps):
            if k % 1 == 0:
                print('Step:', k)
            delta = 2 * np.random.randint(0, 2, size=p) - np.ones(p, dtype=np.int)
            delta = np.array(delta, dtype=np.float32) / 2
            if self.scale is not None:
                delta = delta * self.scale
            pi = np.floor(theta) + np.ones(p, dtype=np.float32) / 2
            tp = np.rint(pi + delta)
            fp = f(tp, config)
            if k % 1 == 0:
                print('tp:', tp, 'fp:', fp)
            tm = np.rint(pi - delta)
            fm = f(tm, config)
            if k % 1 == 0:
                print('tm:', tm, 'fm:', fm)
            gk = (fp - fm) * delta
            if self.scale is not None:
                gk = gk / self.scale
            if self.gmax is not None:
                gk = max(-self.gmax, min(self.gmax, (fp - fm))) * delta
            ak = self.smalla / math.pow(1 + self.biga + k, self.alpha)
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
            self.report(theta)
        return rtheta

    def report(self, vec, title=None, file='report.txt'):
        if title is None:
            title = 'Current best:'
        if file is None:
            repf = sys.stdout
        else:
            repf = open(file, 'w', encoding='utf-8')
        print(title, file=repf)
        for n, v in zip(self.pnames, list(vec)):
            print(n, '=', v, file=repf)
        if file is None:
            repf.close()

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
          ('kingPawn1'     , 8, 48, 1),
          ('kingPawn2'     , 12, 64, 1),
          ('rookHOpen'     , 160, 180, 4),
          ('rookOpen'      , 211, 186, 4),
          ('rookConn'      , 94,  53, 2),
          ('mobilityKnight', 46, 61, 1),
          ('mobilityBishop', 52, 29, 1),
          ('mobilityRook'  , 23, 25, 1),
          ('mobilityQueen' ,  4,  3, 1),
          ('centerPAtts'   , 76, 59, 1),
          ('centerNAtts'   , 44, 41, 1),
          ('centerBAtts'   , 52, 38, 1),
          ('centerRAtts'   , 10, 30, 1),
          ('centerQAtts'   ,  4, 55, 1),
          ('centerKAtts'   ,  2, 54, 1),
          #('space'         ,  1,  0, 1),
          ('adversAtts'       ,  2, 14, 1),
          ('isolPawns'     , (-37), (-108), 2),
          ('isolPassed'    , (-51), (-152), 2),
          ('backPawns'     , (-113), (-151), 2),
          ('backPOpen'     , (-23),  (-20), 1),
          ('enpHanging'    , (-21), (-34), 1),
          ('enpEnPrise'    , (-28), (-26), 1),
          ('enpAttacked'   ,  (-6), (-7), 1),
          ('ewWepAttacked'   , 48, 64, 1),
          ('lastLinePenalty', 107, 3, 2),
          ('bishopPair'    , 390, 320, 4),
          ('bishopPawns'   , (-22), (-58), 1),
          ('redundanceRook', (-29), (-61), 1),
          ('rookPawn'      , (-47), (-37), 1),
          ('advPawn5'      ,    4, 118, 2),
          ('advPawn6'      ,  356, 333, 4),
          ('pawnBlockP'    , (-115), (-90), 2),
          ('pawnBlockO'    ,  (-20), (-23), 1),
          ('pawnBlockA'    ,  (-13), (-70), 1),
          ('passPawnLev'   ,  0, 9, 1),
        ]

resre = re.compile(r'End result')
wdlre = re.compile('[() ,]')

# Play a match with a given number of games
# Player 1 is configured with our input point
# Player 2 is an empty config, which is the reference configuration
def play(x, config):
    os.chdir(config.playdir)
    with open('player1.cfg', 'w', encoding='utf-8') as plf:
        for p, v in zip(config.params, x):
            plf.write('%s=%d\n' % (p, v))
    skip = random.randint(0, 25000)
    #print('Skip = %d' % skip)
    args = [config.selfplay, '-m', config.playdir, '-a', 'player1.cfg', '-b', 'player0.cfg',
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
        return (w + 0.5 * d) / (w + d + l)

#def square(x):
#    f = (x[0] * x[0] + 2 * x[1] * x[1])/10 + np.random.randn()
#    return f
#
#def banana(v):
#    x = v[0]
#    y = v[1]
#    f = (1 - x) * (1 - x) + 100 * (y - x*x) * (y - x*x) + np.random.randn()
#    return f

if __name__ == '__main__':
    pnames = []
    pinits = []
    pscale = []
    hasScale = False
    for name, mid, end, scale in paramWeights:
        #scale = scale * 5
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
                    depth=2, games=100,
                    params=pnames)

    if not hasScale:
        scale = None

    # A = 0.1 * M
    # magn of theta <= 0.1
    # we want first steps: 0.5
    # Then: a = 0.5 / 0.1 * sqrt(A+1)
    opt = DSPSA(pnames, pinits, 28, 30, alpha=0.501, scale=pscale, msteps=300, rend=20)
    r = opt.optimize(play, config)
    opt.report(r, title='Optimum', file='optimum.txt')
    opt.report(r, title='Optimum', file=None)