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
        print('scale:', self.scale)
        p = self.theta.shape[0]
        theta = self.theta
        rtheta = np.rint(theta)
        since = 0
        for k in range(self.msteps):
            print('Step:', k)
            delta = 2 * np.random.randint(0, 2, size=p) - np.ones(p, dtype=np.int)
            if self.scale is not None:
                delta = delta * self.scale
            pi = np.floor(theta) + np.ones(p, dtype=np.float32) / 2
            tp = np.rint(pi + delta / 2)
            tm = np.rint(pi - delta / 2)
            print('plus:', tp)
            print('mius:', tm)
            df = f(tp, tm, config)
            gk = df / delta
            ak = self.smalla / math.pow(1 + self.biga + k, self.alpha)
            print('df:', df, 'ak:', ak)
            # Here: + because we maximize!
            theta = theta + ak * gk
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
    Adadelta should maintain different learning rates per dimension, but in our
    case all dimensions would have equal rates, because in every step only
    the sign is different, and we can't break the simmetry.
    Also, our gradient is just an estimate.
    To deal with these problems we maintain an average gradient and work with it
    as if it would be the current one
    """
    def adadelta(self, f, config, mult=1, beta=0.9, gamma=0.9, niu=0.9, eps=1E-8):
        print('scale:', self.scale)
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
            print('delta:', delta)
            pi = np.floor(theta) + np.ones(p, dtype=np.float32) / 2
            tp = np.rint(pi + delta / 2)
            tm = np.rint(pi - delta / 2)
            print('plus:', tp)
            print('mius:', tm)
            df = f(tp, tm, config)
            gk = df / delta
            # niu is for friction
            gm = (beta * gm + (1 - beta) * gk) * niu
            eg2 = gamma * eg2 + (1 - gamma) * gm * gm
            dtheta = np.sqrt((ed2 + eps) / (eg2 + eps)) * gm
            ed2 = gamma * ed2 + (1 - gamma) * dtheta * dtheta
            # Here: + because we maximize!
            theta = theta + mult * dtheta
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

params = [
        #('epMovingMid',  156, 3),
        #('epMovingEnd',  156, 3),
        #('epMaterMinor', 1, 1),
        #('epMaterRook',  4, 1),
        #('epMaterQueen', 13, 1),
        #('epMaterScale', 1, 1),
        #('epMaterBonusScale', 5, 1),
        #('epPawnBonusScale', 8, 1),
        #('epPassKingProx',  13, 1),
        #('epPassBlockO', 11, 1),
        #('epPassBlockA', 17, 1),
        #('epPassMin',    30, 1),
        #('epPassMyCtrl', 6, 1),
        #('epPassYoCtrl', 7, 1),
    ]

weights = [
      #('kingSafe'      , 1, 0, 1),
      #('kingOpen'      , 5, 0, 1),
      #('kingPlaceCent' , 6, 0, 1),
      #('kingPlacePwns' , 400, 200, 10),
      ('kingPawn1'     , 11, 42, 16),
      ('kingPawn2'     , 10, 69, 16),
      #('rookHOpen'     , 167, 183, 32),
      #('rookOpen'      , 205, 179, 32),
      #('rookConn'      , 92,  58, 32),
      #('rook7th'       , 200, 160, 32),
      #('mobilityKnight', 46, 61, 8),
      #('mobilityBishop', 52, 29, 8),
      #('mobilityRook'  , 18, 32, 8),
      #('mobilityQueen' ,  4,  3, 4),
      ('centerPAtts'   , 73, 59, 16),
      #('centerNAtts'   , 44, 41, 16),
      #('centerBAtts'   , 52, 38, 16),
      #('centerRAtts'   , 14, 23, 16),
      #('centerQAtts'   ,  4, 55, 8),
      #('centerKAtts'   ,  2, 54, 8),
      #('space'         ,  1,  0, 1),
      #('adversAtts'       ,  2, 14, 8),
      ('isolPawns'     , -38, -105, 32),
      ('isolPassed'    , -57, -150, 32),
      ('backPawns'     , -105, -148, 32),
      ('backPOpen'     , -23,  -26, 32),
      #('enpHanging'    , (-21), (-34), 8),
      #('enpEnPrise'    , (-28), (-26), 8),
      #('enpAttacked'   ,  (-6), (-7), 8),
      ('wepAttacked'   ,  46, 61, 16),
      #('lastLinePenalty', 107, 3, 32),
      #('bishopPair'    , 390, 320, 16),
      #('bishopPawns'   , (-22), (-58), 16),
      #('redundanceRook', (-27), (-52), 32),
      ('rookPawn'      , -49, -27, 32),
      ('advPawn5'      , 8, 109, 32),
      ('advPawn6'      , 359, 330, 32),
      ('pawnBlockP'    , -117, -95, 32),
      ('pawnBlockO'    , -18, -27, 32),
      ('pawnBlockA'    , -15, -64, 32),
      #('passPawnLev'   ,  2, 8, 1),
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
    if w == None or w + d + l == 0:
        #raise RuntimeError('No result from self play')
        return 0
    else:
        return 2 * (w + 0.5 * d) / (w + d + l) - 1

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
    for name, val, scale in params:
        pnames.append(name)
        pinits.append(val)
        pscale.append(scale)
        if scale != 1:
            hasScale = True

    for name, mid, end, scale in weights:
        pnames.append('mid.' + name)
        pinits.append(mid)
        pscale.append(scale)
        pnames.append('end.' + name)
        pinits.append(end)
        pscale.append(scale)
        if scale != 1:
            hasScale = True

    config = Config(selfplay=r'C:\astra\SelfPlay-r7.exe',
                    playdir=r'C:\astra\play', ipgnfile=r'C:\astra\open-moves\open-moves.fen',
                    depth=4, games=2,
                    params=pnames)

    if not hasScale:
        scale = None

#    # Tests
#    opt = DSPSA(['x', 'y'], [3, 3], 0.03, msteps=10, scale=[3, 3])
#    #r = opt.adadelta(banana_diff, config)
#    r = opt.optimize(square_diff, config)
#    #r = opt.adadelta(square_diff, config)

    # Real
    opt = DSPSA(pnames, pinits, 5.0, msteps=2000, scale=pscale, rend=300)
    r = opt.optimize(play, config)
    #r = opt.momentum(play, config)
    #r = opt.adadelta(play, config, mult=20, beta=0.995, gamma=0.995, niu=0.999, eps=1E-8)
    opt.report(r, title='Optimum', file='optimum.txt')
    opt.report(r, title='Optimum', file=None)