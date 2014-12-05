/// A most bare-bone Lens module
module Logary.Lenses

/// Basic Lens-record specifying what a lens is.
type Lens<'a,'b> =
  /// Get the value of this lens, given the object to read the value from
  { get : 'a -> 'b
  /// Sets the value of this lens, given the new value and the object
  /// to create an updated copy of
  ; set : 'b -> 'a -> 'a }
with
  /// first get the value of the lens, f(value), write it back.
  member l.update f a =
    let value = l.get a
    let newValue = f value
    l.set newValue a

/// Combine two lenses as an even more focused lens.
let inline (<|>) (l1: Lens<_,_>) (l2: Lens<_,_>) =
  { get = l1.get >> l2.get
    set = l2.set >> l1.update }