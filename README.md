# virtual-hom
Haskell+GHCJS implementation of [virtual-dom][https://github.com/Matt-Esch/virtual-dom]. It's not a direct port, I merely copied the idea (a virtual DOM with incremental updates).

Comparison of virtual-dom parts with their equivalents in virtual-hom:

- **vdom**, a render and patch algorithm for vtree: See `prepare` and `render` in `Rendering.hs`
- **vtree**, a realtime diff algorithm: See `diff` in `Rendering.hs`. Note that I haven't made any performance measurements so I do not claim my implementation is "realtime"
- **virtual-hyperscript**, a DSL for creating virtual trees: Not really a DSL here, it's the `Elem cb a` data type in `Element.hs`, and constructors in `Html.hs`

## Status of Project

The virtual-dom part is functional. However, since I've built this to support another project of mine, I haven't spent much time on providing a complete list of constructors for all dom elements. So it definitely needs some polishing before it can be released properly. In the meantime, you can build any kind of element directly, look at `Html.hs` for examples.

TO DO:
[ ] Add constructors for remaining HTML5 elements to `Html.hs`
[ ] Improve callbacks (more callbacks, callbacks with arguments)
[ ] Support a user-defined `key` property to allow re-ordering of child elements instead of mutation
[ ] Remove `lens` dependency?
[ ] Decide whether to use JSString everywhere, instead of Text

## How to build

```
stack setup
stack build
```

To generate the haddocks, run `stack --stack-yaml stack-ghc.yaml haddock`. To build the examples, run `stack build --flag virtual-hom:examples`

## License

BSD3

## Contributions

Bug reports, pull requests, feature requests are welcome
