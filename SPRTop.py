# Optimize evaluation parameters of a chess engine by a combination of DSPSA and SPRT
# Inputs:
# - parameters to optimize, every parameter having a current (start) value and a param step value
# - SPRT parameters
# - an update step constant (see below)
# - a total number of games to be played
# Steps:
# 1. generate a new (random) point in parameter space, by modifying each parameter as:
# param current value +/- param step (+ or - is the random part)
# 2. run a SPRT between the current point and the new point
# 3. if SPRT succeded (i.e. the new is better):
#    - calculate current by making a small step in the direction of the new point
#    - step size proportional to update step constant and inverse to the SPRT length (number of played games)
# 3. Did we play all allocated games?
#    - if no: go to step 1
#    - if yes: end, the current point is the optimum

import numpy as np
import random
import math
import subprocess
import re
import os
import os.path
import sys

# A config class of the problem to be optimized
class Config:
    # These are acceptable fields in section 0, with theire type
    # S is string, I integer and F float
    fields = {
        'selfplay': 'S',    # program name for self play
        'playdir': 'S',     # directory where to play the games
        'ipgnfile': 'S',    # input pgn file
        'depth': 'I',       # the games are fixed depth with this depth
        'games': 'I',       # so many games played at once per selfplay launch
        'sprtgames': 'I',   # maximal SPRT games to play (if not passed, is failed)
        'totalgames': 'I',  # so many games to be playes in total (at least, can be a little more)
        'stepconst': 'F',   # step constant
        'elo0': 'F',        # SPRT parameters
        'elo1': 'F',
        'alpha': 'F',
        'beta': 'F'
    }

    def __init__(self, filename):
        if not os.path.exists(filename):
            raise Exception('Config file {} does not exist'.format(filename))
        # Parameters for playing
        self.selfplay = ''
        self.playdir = ''
        self.ipgnfile = ''
        self.depth = 4
        self.games = 8
        self.sprtgames = 4096
        self.totalgames = 32768
        # The parameters to optimize + start point
        self.pnames = []
        self.pinits = []
        self.psteps = []
        # Optimization hyper parameters
        self.stepconst = 8
        self.elo0 = 0
        self.elo1 = 1
        self.alpha = 0.05
        self.beta = 0.05
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

        # Calculate lowerllr and upperllr
        self.lowerllr = math.log(self.beta / (1.0 - self.alpha))
        self.upperllr = math.log((1.0 - self.beta) / self.alpha)

        # Collect the eval parameters
        for name, vals in sectionNames[0].items():
            val = vals[0]
            step = vals[1]
            self.pnames.append(name)
            self.pinits.append(val)
            self.psteps.append(step)

        # Collect the eval weights
        for name, vals in sectionNames[1].items():
            mid = vals[0]
            end = vals[1]
            step = vals[2]
            self.pnames.append('mid.' + name)
            self.pinits.append(mid)
            self.psteps.append(step)
            self.pnames.append('end.' + name)
            self.pinits.append(end)
            self.psteps.append(step)

resre = re.compile(r'End result')
wdlre = re.compile('[() ,]')

# Play a match with a given number of games between 2 engine theta
# tp is the challenger, tm is the old guy
def play(config, tp, tm):
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
        print('Warning, no result from self play!')
        return 0, 0, 0
    else:
        return w, d, l

def optimize(config):
    current_theta = np.array(config.pinits, dtype=np.float32)
    p = current_theta.shape[0]
    param_steps = np.array(config.psteps, dtype=np.float32)
    total_games = 0
    while total_games < config.totalgames:
        tm = np.rint(current_theta)
        delta = (2 * np.random.randint(0, 2, size=p) - np.ones(p, dtype=np.int)) * param_steps
        tp = np.rint(tm + delta)
        print('\nTotal games:', total_games)
        print('curr:', tm)
        print('next:', tp)
        games, passed = play_sprt(config, tp, tm)
        print('games =', games, ', passed =', passed)
        if passed:
            alpha = config.stepconst / games
            print('alpha =', alpha)
            current_theta += (tp - current_theta) * alpha
        total_games += games
        if passed:
            report_optimum(config, current_theta, title='Optimum after {:d} games'.format(total_games))
    return np.rint(current_theta)

def play_sprt(config, tnew, told):
    games = 0
    wt, dt, lt = 0, 0, 0
    finished = False
    while games < config.sprtgames and not finished:
        w, d, l = play(config, tnew, told)
        wt += w
        dt += d
        lt += l
        print('selfplay returned:', w, d, l, ', total:', wt, dt, lt)
        games = wt + dt + lt
        sprt = SPRT(wt, lt, dt, config.elo0, config.elo1)
        print('sprt =', sprt, ', lower =', config.lowerllr, ', upper =', config.upperllr)
        passed = sprt > config.upperllr
        failed = sprt < config.lowerllr
        finished = passed or failed
    return games, passed

def report_optimum(config, vec, title=None, file='report.txt'):
    if title is None:
        title = 'Current best:'
    if file is None:
        print(title)
        for n, v in zip(config.pnames, list(vec)):
            print(n, '=', v)
    else:
        with open(file, 'w', encoding='utf-8') as repf:
            print(title, file=repf)
            for n, v in zip(config.pnames, list(vec)):
                print(n, '=', v, file=repf)

# These functions: SPRT, bayeselo_to_proba, proba_to_bayeselo, erf_inv, phi_inv and ELO:
# copyright by Andy Grant, see https://github.com/AndyGrant/OpenBench/blob/master/OpenBench/utils.py
def SPRT(wins, losses, draws, elo0, elo1):

    # Estimate drawelo out of sample. Return LLR = 0.0 if there are not enough
    # games played yet to compute an LLR. 0.0 will always be an active state
    if wins > 0 and losses > 0 and draws > 0:
        N = wins + losses + draws
        _, drawelo = proba_to_bayeselo(float(wins)/N, float(draws)/N, float(losses)/N)
    else: return 0.00

    # Probability laws under H0 and H1
    p0win, p0draw, p0loss = bayeselo_to_proba(elo0, drawelo)
    p1win, p1draw, p1loss = bayeselo_to_proba(elo1, drawelo)

    # Log-Likelyhood Ratio
    return    wins * math.log(p1win  /  p0win) \
          + losses * math.log(p1loss / p0loss) \
          +  draws * math.log(p1draw / p0draw)

def bayeselo_to_proba(elo, drawelo):
    pwin  = 1.0 / (1.0 + math.pow(10.0, (-elo + drawelo) / 400.0))
    ploss = 1.0 / (1.0 + math.pow(10.0, ( elo + drawelo) / 400.0))
    pdraw = 1.0 - pwin - ploss
    return pwin, pdraw, ploss

def proba_to_bayeselo(pwin, pdraw, ploss):
    elo     = 200 * math.log10(pwin/ploss * (1-ploss)/(1-pwin))
    drawelo = 200 * math.log10((1-ploss)/ploss * (1-pwin)/pwin)
    return elo, drawelo

def erf_inv(x):
    a = 8*(math.pi-3)/(3*math.pi*(4-math.pi))
    y = math.log(1-x*x)
    z = 2/(math.pi*a) + y/2
    return math.copysign(math.sqrt(math.sqrt(z*z - y/a) - z), x)

def phi_inv(p):
    # Quantile function for the standard Gaussian law: probability -> quantile
    assert(0 <= p and p <= 1)
    return math.sqrt(2)*erf_inv(2*p-1)

def ELO(wins, losses, draws):

    def _elo(x):
        if x <= 0 or x >= 1: return 0.0
        return -400*math.log10(1/x-1)

    # win/loss/draw ratio
    N = wins + losses + draws
    if N == 0: return (0, 0, 0)
    w = float(wins)  / N
    l = float(losses)/ N
    d = float(draws) / N

    # mu is the empirical mean of the variables (Xi), assumed i.i.d.
    mu = w + d/2

    # stdev is the empirical standard deviation of the random variable (X1+...+X_N)/N
    stdev = math.sqrt(w*(1-mu)**2 + l*(0-mu)**2 + d*(0.5-mu)**2) / math.sqrt(N)

    # 95% confidence interval for mu
    mu_min = mu + phi_inv(0.025) * stdev
    mu_max = mu + phi_inv(0.975) * stdev

    return (_elo(mu_min), _elo(mu), _elo(mu_max))

if __name__ == '__main__':

    confFile = sys.argv[1]
    config = Config(confFile)

    r = optimize(config)
    pref, suff = os.path.split(confFile)
    report_optimum(config, r, title='Optimum', file=os.path.join(pref, 'optimum-' + suff))
    report_optimum(config, r, title='Optimum', file=None)