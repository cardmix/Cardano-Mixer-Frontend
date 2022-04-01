# Cardano Mixer Frontend

## Nix setup

My `/etc/nix/nix.conf` file:

```
binary-caches = https://cache.nixos.org/ https://nixcache.reflex-frp.org https://hydra.iohk.io
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI= b2builder:z/WvwKa+KSs8oN88xpTnAzzKnlMS4uoXZHzd9XKyqdY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
binary-caches-parallel-connections = 40
sandbox = true
max-jobs = 16
cores = 8
```

It enables caches so that you don't have to rebuld a lot of things every time.

## Building Reflex frontend

Run this command to start nix shell (it may take some time):

```
nix-shell -E '(import ./default.nix { }).shells.ghcjs'
```

In the shell, run `cabal build --ghcjs -f frontend` to build it.

Copy `all.js` file to `result/`:

```
cp dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/cardanomixer-frontend-0.1.0.0/x/cardanomixer/build/cardanomixer/cardanomixer.jsexe/all.js result/
```

## Setup for local development

Download [`caddy2`](https://caddyserver.com/v2).

Run the server locally `caddy run`.

Open http://localhost:3333/ in the browser to see results!
