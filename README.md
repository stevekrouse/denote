# denote

Currently working on replicating [RealWorld](https://github.com/gothinkster/realworld) in Reflex without any "backend". I'm simulating multiple browsers by simply making two browser windows in the same UI window.

There's a [Refelx + Obelisk implemention of RealWorld](https://github.com/qfpl/reflex-realworld-example), which is very helpful, particularly for copying-and-pasting DOM layout code. However, I'm currently not using a lot of their State Monads (for getting and setting various database-level values); instead I'm threading this values manually throughout the program to avoid the "global state everywhere" problem. I think I'll likely get fed up with this strategy quite soon and adopt some similar abstraction for keeping track of URL-bar and State updates.

I have a [Turbine / Hareactive program in a similar state](https://codesandbox.io/s/denote-conduit-g08mb?file=/src/index.ts:3297-3304).

## Files

`Main.hs` - current state of the program: keeps track of users across "browsers" and URL-changes within any browser

`Main-pre-router.hs` is what the program looked like before I had to account for routing. (A lot simpler, I think)

## VS Code

* Command-Shift-P: nix-select-environment: shells.nix (and refresh when prompted)
* Haskell extension is great!

## Running things

* `reflex-platform/try-reflex`
* `cabal run denote`

## Resources

* https://github.com/reflex-frp/reflex-dom/blob/develop/Quickref.md
* https://github.com/reflex-frp/reflex/blob/develop/Quickref.md
* https://reflex-frp.org/tutorial
