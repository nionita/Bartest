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
import os.path
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

# Transforming mean result in an elo difference involves the constants log(10) and 400,
# but at the end those are multiplicative, so it is just a matter of step size
# Here frac is between 0 and 1, exclusive! More than 0.5 means the first player is better
# We have guards for very excentric results, eps0 and eps1, which also limit the gradient amplitude:
eps0 = 1e-2
eps1 = 1 - eps0

def elowish(frac):
    frac = max(eps0, min(eps1, frac))
    return -math.log(1 / frac - 1)

"""
Implementation of DSPSA
"""
class DSPSA:
    def __init__(self, config):
        self.config = config
        self.pnames = config.pnames
        self.smalla = config.laststep * math.pow(1.1 * config.msteps + 1, config.alpha)
        self.biga = 0.1 * config.msteps
        self.alpha = config.alpha
        self.msteps = config.msteps
        self.theta = np.array(config.pinits, dtype=np.float32)
        if config.pscale is None:
            self.scale = None
        else:
            self.scale = np.array(config.pscale, dtype=np.float32)
        self.rend = config.rend

    def optimize(self, f):
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
            df = f(tp, tm, self.config)
            gk = df / delta
            # Is this rescale better? (added on 8th May 2018)
            if self.scale is not None:
                gk = gk * self.scale
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
    # These are acceptable fields in section 0, with theire type
    # S is string, I integer and F float
    fields = {
        'selfplay': 'S',
        'playdir': 'S',
        'ipgnfile': 'S',
        'depth': 'I',
        'games': 'I',
        'laststep': 'F',
        'alpha': 'F',
        'msteps': 'I',
        'rend': 'I'
    }

    def __init__(self, filename):
        if not os.path.exists(filename):
            raise Exception('Config file {} does not exist'.format(filename))
        # Parameters for playing
        self.playdir = ''
        self.depth = 4
        self.games = 16
        # The parameters to optimize + start point
        self.pnames = []
        self.pinits = []
        self.pscale = []
        # Optimization hyper parameters
        self.laststep = 0.1
        self.alpha=0.501
        self.msteps=1000
        self.rend=None
        self._readConfig(filename)

    def _readConfig(self, conffile):
        section = 0
        lineno = 0
        error = False
        seen = set()
        sectionNames = [dict(), dict()]
        with open(conffile, 'r') as cof:
            for line in cof:
                lineno += 1
                # split the comment path
                line = re.split('#', line)[0].lstrip().rstrip()
                if len(line) > 0:
                    if line == '[params]':
                        section = 1
                    elif line == '[weights]':
                        section = 2
                    else:
                        parts = re.split(':\s*', line, 1)
                        name = parts[0]
                        val = parts[1]
                        if section == 0:
                            if name in self.fields:
                                if self.fields[name] == 'S':
                                    self.__setattr__(name, val)
                                elif self.fields[name] == 'I':
                                    self.__setattr__(name, int(val))
                                elif self.fields[name] == 'F':
                                    self.__setattr__(name, float(val))
                                else:
                                    raise Exception('Wrong field type in Config class: {:s}'.format(self.fields[name]))
                            else:
                                print('Config error in line {:d}: unknown config name {:s}'.format(lineno, name))
                                error = True
                        else:
                            vals = re.split(',\s*', val)
                            if len(vals) == section + 1:
                                if name in seen:
                                    print('Config error in line {:d}: name {:s} already seen'.format(lineno, name))
                                    error = True
                                else:
                                    seen.add(name)
                                    sectionNames[section-1][name] = [int(v) for v in vals]
                            else:
                                print('Config error in line {:d}: should have {:d} values, it has {:d}'.format(lineno, section+1, len(vals)))
                                error = True
        if error:
            raise Exception('Config has errors')
        hasScale = False

        # Collect the eval parameters
        for name, vals in sectionNames[0].items():
            val = vals[0]
            scale = vals[1]
            self.pnames.append(name)
            self.pinits.append(val)
            self.pscale.append(scale)
            if scale != 1:
                hasScale = True

        # Collect the eval weights
        for name, vals in sectionNames[1].items():
            mid = vals[0]
            end = vals[1]
            scale = vals[2]
            self.pnames.append('mid.' + name)
            self.pinits.append(mid)
            self.pscale.append(scale)
            self.pnames.append('end.' + name)
            self.pinits.append(end)
            self.pscale.append(scale)
            if scale != 1:
                hasScale = True

        if not hasScale:
            self.pscale = None

resre = re.compile(r'End result')
wdlre = re.compile('[() ,]')

# Play a match with a given number of games between theta+ and theta-
# Player 1 is theta+
# Player 2 is theta-
def play(tp, tm, config):
    # print('chdir to', config.playdir)
    os.chdir(config.playdir)
    with open('playerp.cfg', 'w', encoding='utf-8') as plf:
        for p, v in zip(config.pnames, tp):
            plf.write('%s=%d\n' % (p, v))
    with open('playerm.cfg', 'w', encoding='utf-8') as plf:
        for p, v in zip(config.pnames, tm):
            plf.write('%s=%d\n' % (p, v))
    skip = random.randint(0, 25000)
    #print('Skip = %d' % skip)
    args = [config.selfplay, '-m', config.playdir, '-a', 'playerp.cfg', '-b', 'playerm.cfg',
            '-i', config.ipgnfile, '-d', str(config.depth), '-s', str(skip), '-f', str(config.games)]
    # print('Will start:')
    # print(args)
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
        return elowish((w + 0.5 * d) / (w + d + l))

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

    # config = Config(selfplay=r'C:\astra\SelfPlay-tbk.exe',
    #                 playdir=r'C:\astra\play', ipgnfile=r'C:\astra\open-moves\open-moves.fen',
    #                 depth=4, games=2,
    #                 params=pnames)
    confFile = sys.argv[1]
    config = Config(confFile)
    # print(config.pnames)
    # print(config.pinits)

#    # Tests
#    opt = DSPSA(['x', 'y'], [3, 3], 0.03, msteps=10, scale=[3, 3])
#    #r = opt.adadelta(banana_diff, config)
#    r = opt.optimize(square_diff, config)
#    #r = opt.adadelta(square_diff, config)

    # Real
    opt = DSPSA(config)
    r = opt.optimize(play)
    #r = opt.momentum(play, config)
    #r = opt.adadelta(play, config, mult=20, beta=0.995, gamma=0.995, niu=0.999, eps=1E-8)
    pref, suff = os.path.split(confFile)
    opt.report(r, title='Optimum', file=os.path.join(pref, 'optimum-' + suff))
    opt.report(r, title='Optimum', file=None)