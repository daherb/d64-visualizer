# D64 Visualizer

Inspired by (https://chaos.social/@c64_disks@botsin.space)[C64DiskBot]
(Source https://github.com/SuperIlu/C64DiskBot/tree/main)

Run using stack:

```
stack build && stack run -- -l -s app/Main.hs -o ddt.svg 
```

The input filename is currently hardcoded but can be updated on the fly (the
`-l` parameter triggers automatic rebuild when the code is changed)
