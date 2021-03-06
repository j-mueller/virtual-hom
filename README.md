# virtual-hom 
[![Build Status](https://travis-ci.org/j-mueller/virtual-hom.svg?branch=master)](https://travis-ci.org/j-mueller/virtual-hom)

Haskell+GHCJS implementation of [virtual-dom](https://github.com/Matt-Esch/virtual-dom), mixed with a lens-based API for creating and composing user interfaces.

```[haskell]
data AppState = Loading (...) | Error (...) | Done (...)
makePrisms ''AppState


stateView :: View Identity AppState
stateView =
  (on _Loading loadingView) <>
  (on _Error   errorView)   <>
  (on _Done    doneView)
```

```[haskell]
theUI :: View Identity Int
theUI i = [container & children .~ [
  row & children .~ [
    h1 "Hello, world",
    p & content .~ "I am a paragraph!",
    p & content .~ ("This button has been clicked " <> (show i) <> " times"),
    btnDefault 
      & content .~ "Submit"
      & callbacks . click ?~ (\_ -> return . succ)]
    ]
  ]
```

## Documentation

- Slides from my lightning talk at Haskell Exchange 2016: https://github.com/j-mueller/talks/blob/master/2016-10-haskell-exchange/2016-10-07-virtual-hom.pdf
- Example project (ready to build): https://github.com/j-mueller/virtual-hom-example

## Status of Project

The virtual-dom part is functional. However, since I've built this to support another project of mine, I haven't spent much time on providing a complete list of constructors for all dom elements. So it definitely needs some polishing before it can be released properly. In the meantime, you can build any kind of element directly, look at `Html.hs` for examples.

### Comparison of virtual-dom parts with their equivalents in virtual-hom:

- **vdom**, a render and patch algorithm for vtree: See `prepare` and `render` in `Rendering.hs`
- **vtree**, a realtime diff algorithm: See `diff` in `Rendering.hs`. (Not sure if it qualifies as "realtime")
- **virtual-hyperscript**, a DSL for creating virtual trees: Not really a DSL here, it's the `Elem cb a` data type in `Element.hs`, and constructors in `Html.hs`

### TO DO:

- [ ] Support a user-defined `key` property to allow re-ordering of child elements instead of mutation
- [ ] Decide whether to use JSString everywhere, instead of Text
- [ ] XmlHTTPRequest example with ghcjs-dom
- [ ] Performance improvements

### DONE: 

- [X] Fix bug that results in elements being rendered in the wrong order
- [x] Add constructors for remaining HTML5 elements to `Html.hs`
- [x] Improve callbacks (more callbacks)
- [x] Improve callbacks (callbacks with arguments)


## How to build

```
stack setup
stack build
```

* To generate the haddocks, `stack --stack-yaml stack-ghc.yaml haddock`
* To build the examples, `stack build --flag virtual-hom:examples`
* To run the tests, `stack test --stack-yaml=stack-ghc.yaml`

## License

BSD3

## Contributions

Bug reports, pull requests, feature requests are welcome


## Ideas etc

* Prisms for validation 