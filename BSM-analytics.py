import numpy as np
from math import sqrt, pi, log, e
from enum import Enum
import scipy.stats as stat
from scipy.stats import norm
import time


class BSMerton:
    def __init__(self, args):
        self.Type = int(args[0])                # 1 for a a call, -1 for a put
        self.S = float(args[1])                 # underlying asset price
        self.K = float(args[2])                 # option strike K
        self.r = float(args[3])                 # continuous risk free rate
        self.q = float(args[4])                 # dividend continuous rate
        self.T = float(args[5]) / 365.0         # compute time to expiry
        self.sigma = float(args[6])             # underlying volatility
        self.sigmaT = self.sigma * self.T ** 0.5# sigma*T for reusability
        self.d1 = (log(self.S / self.K) +
                    (self.r - self.q + 0.5 * (self.sigma ** 2))
                    * self.T) / self.sigmaT
        self.d2 = self.d1 - self.sigmaT
        [self.Delta] = self.delta()

    def delta(self):
        dfq = e ** (-self.q * self.T)
        if self.Type == 1:
            return [dfq * norm.cdf(self.d1)]
        else:
            return [dfq * (norm.cdf(self.d1) - 1)]
