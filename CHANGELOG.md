# `chainweb-miner` Changelog

## 1.1.0 (2020-02-27)

- The last 8 bytes of a Block Header may also be interpreted as a Nonce. This allows
  for a significant performance improvement for GPU miners.

## 1.0.3 (2019-12-01)

#### New Features

- Improved `chainweb-node` connection behaviour.
  ([#13](https://github.com/kadena-io/chainweb-miner/pull/13))
- `testnet04` support.

#### Bug Fixes

- Fixed `chainweb-miner balance` to account for changes in Chainweb.
  ([#19](https://github.com/kadena-io/chainweb-miner/pull/19))

## 1.0.2 (2019-11-21)

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
