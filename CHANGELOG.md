# `chainweb-miner` Changelog

## 1.0.2 (2019-11-??)

#### New Features

- **New Command:** `chainweb-miner balance` to check the balance of a given
  account. ([#1](https://github.com/kadena-io/chainweb-miner/pull/1))

#### Bug Fixes

- Connect to `chainweb-node` in a way that pressures its network stack less. See
  also [this issue](https://github.com/kadena-io/chainweb-node/issues/687).
  ([#9](https://github.com/kadena-io/chainweb-miner/pull/9))

## 1.0.1 (2019-11-14)

#### Bug Fixes

- It is now impossible to pass illegal Public Keys to `--miner-key`.
