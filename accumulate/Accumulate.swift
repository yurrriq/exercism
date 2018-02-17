//
//  Accumulate.swift
//  Exercism
//
//  Created by Eric Bailey on 6/7/16.
//
//

public extension CollectionType {

  /**
   Return an `Array` containing the results of mapping `transform` over `self`.

   - parameter transform: A throwing function from `Self.Generator.Element` to `T`.
   - throws: Any error thrown by a call to `transform`.
   - returns: The resulting `[T]`.
   */
  @warn_unused_result
  func accumulate<T>(@noescape transform: (Self.Generator.Element) throws -> T) rethrows -> [T] {
    var acc = [T]()
    for x in self {
      try acc.append(transform(x))
    }
    return acc
  }

}
