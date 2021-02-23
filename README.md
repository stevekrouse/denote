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

* `. /home/ubuntu/.nix-profile/etc/profile.d/nix.sh` 
* `reflex-platform/try-reflex`
* `cabal run denote`

## Resources

* https://github.com/reflex-frp/reflex-dom/blob/develop/Quickref.md
* https://github.com/reflex-frp/reflex/blob/develop/Quickref.md
* https://reflex-frp.org/tutorial
* `nix-shell`, then `hoogle server --local`
## Open questions

1. How do I handle / simulate the time it takes for information to propogate between browsers? (loading time)
2. Ditto for a losing a network connection?

I can see a couple approaches here:

a) All combinators that cross between locations (browsers) can encode this delay and failure-possibility in their types. This would give the programmer the most fine-grained control over what they want to happen in response to various network behavior. The downside, of course, is that this may be cumbersome and remove some of the simplicity from programs. However maybe there's a way to have it live in the background, propogating up the chain until someone handles it.

b) It can only be exposed to the programmer at the topmost level, allowing them the ability to specify what the error message for the whole page should be when the network connectivity isn't sufficient for the semantics to be met. In other words, a custom error message for "you're not sufficiently online".

Not-so-random aside: how do we handle optimisitic updates? You have a local type, that gets lifted to a shared type, which then comes back to the browser in a local context. This round trip takes time. The easiest thing would be to wait it out and just use the shared type when it coems back. However if you want to make things snappier, there should be a way to update your local copy of the shared type immediately, and then once the new shared type comes in, we merge that in as seamlessly as we can.

Another cool approach is the CRDT one as long as we limit how our state can evolve appropriately... Adriaan's work wil be super relevant here.

3. Curious to see how I handle more and more shared, global states between components over time... As said above I'm relucant for State monads because they feel too imperitive, but we shall see where things go.
