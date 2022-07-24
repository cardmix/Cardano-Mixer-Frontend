# Cardano Mixer Frontend

## Building Reflex frontend

In the shell, run
```
cabal new-build --ghcjs
```
to build it.

Copy `all.js` file to `result/`:

```
cp dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/cardanomixer-frontend-0.1.0.0/x/cardanomixer/build/cardanomixer/cardanomixer.jsexe/all.js result/
```

## Setup for local development

Download [`caddy2`](https://caddyserver.com/v2).

Run the server locally `caddy run`.

Open http://localhost:3333/ in the browser to see the results!
