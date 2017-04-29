# -*- coding: utf-8 -*-
"""
Created on Mon Apr 24 15:59:24 2017

@author: nicu
"""

import numpy as np
import cv2
import random
import math
import subprocess
import re
import os

# Stochastic optimisation with Thompson sampling
# We have to optimize a stochastic function of n parameters,
# but we can only get random results from an experiment (match)
# which delivers only the values 0 or 1
# The expectation of the result is proportional to the function value

# Transform Elo to fraction and back
ln10p400 = -math.log(10.0) / 400.0
iln10p400 = 1. / ln10p400
                    
def elo2frac(elo):
    return 1.0 / (1.0 + math.exp(elo * ln10p400))

def frac2elo(frac):
    return math.log(1. / frac - 1.) * iln10p400

# A class to help with the Thompson sampling
class Thompson:
    thid = 0

    def __init__(self):
        self.zeros = 0
        self.ones  = 0
        Thompson.thid += 1
        self.thid  = Thompson.thid

    def simulate(self):
        return random.betavariate(self.ones+1, self.zeros+1)

    def played(self):
        return self.zeros + self.ones

    def score(self):
        p = self.played()
        if p == 0 or self.ones == 0:
            return 0., -math.inf
        elif self.ones == p:
            return 1., math.inf
        else:
            r = self.ones / p
            return r, frac2elo(r)

    def prec(self):
        p = self.played()
        if p == 0:
            return math.inf
        else:
            return 500. / math.sqrt(p)

    def lowscore(self):
        p = self.played()
        if p == 0 or self.ones == 0:
            return 0., -math.inf
        elif self.ones == p:
            return 1., math.inf
        else:
            r = self.ones / p
            m = 500. / math.sqrt(p)
            return r - elo2frac(m), frac2elo(r) - m

    def reward(self, won, lost):
        self.ones += won
        self.zeros += lost

# The optimisation: we make a simplified one first, to check the concept
# It optimizes in integers
class Optimizer:
    def __init__(self, x, steps=1000, radius=3, bandits=None):
        if not isinstance(x, tuple):
            raise(TypeError)
        self.dims = len(x)
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

    def step_prep(self):
        best = None
        bestv = -math.inf
        for x in self.simp:
            v = self.results[x].simulate()
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
        self.results[self.sbest].reward(w, l)
        t = self.results[self.sbest]
        s, _ = t.score()
        if self.debug:
            print('Step play:', self.sbest, s, 'in', t.played(), '(', t.thid, ')')
        self.newbest()
        return w, l, s

#    def allbests(self):
#        bests = []
#        bestv = -math.inf
#        bestp = -math.inf
#        for x, t in self.results.items():
#            v, _ = t.score()
#            p = t.played()
#            if v > bestv:
#                bests = [x]
#                bestv = v
#                bestp = p
#            elif v == bestv:
#                if p > bestp:
#                    bests = [x]
#                    bestp = p
#                elif p == bestp:
#                    bests.append(x)
#        return bests, bestv, bestp

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

class Config:
    def __init__(self, selfplay='', playdir='.', ipgnfile='', depth=4, games=16, params=[]):
        self.selfplay = selfplay
        self.playdir = playdir
        self.ipgnfile = ipgnfile
        self.depth = depth
        self.games = games
        self.params = params

config = Config(selfplay=r'C:\astra\SelfPlay-self.exe',
                playdir=r'C:\astra\play', ipgnfile=r'C:\astra\open-moves\open-moves.fen',
                depth=4, games=4, params=['epMovingMid', 'epMovingEnd'])

resre = re.compile(r'End result')
wdlre = re.compile('[() ,]')

# Play a match with a given number of games
# Player 1 is configured with our input point
# Player 2 is an empty config, which is the reference configuration
def play(x):
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
                #d = int(ds)
                l = int(ls)
                #print('I found the result %d, %d, %d' % (w, d, l))
    if w == None:
        raise RuntimeError('No result from self play')
    else:
        return w, l

class Visualize:
    fact = 20
    xorg = 320
    yorg = 320
    ca = 255. * 3. / 2.
    
    def __init__(self, p, xm):
        x, y = p
        self.ox = x
        self.oy = y
        self.mi = -xm
        self.ma = xm
    
    def coord(self, x, y, lat, s):
        x = x - self.ox
        y = y - self.oy
        if x >= self.mi and x <= self.ma and y >= self.mi and y <= self.ma:
            xi =       x * Visualize.fact + Visualize.xorg
            yi = 640 - y * Visualize.fact - Visualize.yorg
            return True, xi - lat, yi - lat, xi + lat, yi + lat
        else:
            return False, 0, 0, 0, 0
        
    def color(self, s):
        r = min(255, int(Visualize.ca * s))
        return (r, r, r)

if __name__ == '__main__':
    x0, y0 = 156, 147
    x = (x0, y0)
    viz = True
    if viz:
        img = np.zeros((640, 640, 3), np.uint8)
        visual = Visualize(x, 15)
        _, xc1, yc1, xc2, yc2 = visual.coord(x0, y0, 315, 1)
        c = visual.color(1)
        img = cv2.rectangle(img, (xc1, yc1), (xc2, yc2), c, 1)
        cv2.imshow('img', img)
        k1 = cv2.waitKey(1) & 0xFF
    opt = Optimizer(x, steps=5, radius=3, bandits=4)
    i = 0
    maxi = 2000
    while i < maxi:
        i = i + 1
        x, y = opt.step_prep()
        if viz:
            draw, xc1, yc1, xc2, yc2 = visual.coord(x, y, 9, 0)
            if draw:
                img = cv2.rectangle(img, (xc1, yc1), (xc2, yc2), (0, 255, 0), -1)
                cv2.imshow('img', img)
                if cv2.waitKey(1) & 0xFF == ord('q'):
                    break
        w, l, s = opt.step_exec()
        if i%10 == 0:
            print('%6d games' % i)
        if viz:
            if draw:
                c = visual.color(s)
                img = cv2.rectangle(img, (xc1, yc1), (xc2, yc2), c, -1)
                cv2.imshow('img', img)
            if cv2.waitKey(1) & 0xFF == ord('q'):
                break

    print('Max steps reached')
    bests, bestv = opt.allbests()
    print('All bests:', bestv)
    for x in bests:
        t = opt.results[x]
        s, _ = t.score()
        print(' -', x, ': score = %.3f, played = %d (%d)' % (s, t.played(), t.thid))
    bests, bestv = opt.lowbests()
    print('Low bests:', bestv)
    for x in bests:
        t = opt.results[x]
        _, s = t.lowscore()
        print(' -', x, ': score = %.3f, played = %d (%d)' % (s, t.played(), t.thid))
    most = []
    mpld = 0
    for x in opt.results:
        t = opt.results[x]
        p = t.played()
        if p > mpld:
            most = [x]
            mpld = p
        elif p == mpld:
            most.append(x)
    print('Most played:', mpld)
    for x in most:
        t = opt.results[x]
        s, _ = t.score()
        print(' -', x, ': score = %.3f, played = %d (%d)' % (s, t.played(), t.thid))
    print('Total players:', len(opt.results), '(', maxi/len(opt.results), ')')
    if viz:
        print('Press any key')
        cv2.waitKey(0)
        cv2.destroyAllWindows()
