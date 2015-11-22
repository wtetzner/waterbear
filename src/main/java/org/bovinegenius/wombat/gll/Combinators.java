package org.bovinegenius.wombat.gll;

public class Combinators {
  public static <I,T,E> Parser<I,T,E> alt(Parser<I,T,E> left, Parser<I,T,E> right) {
    return (input, cont) -> {
        left.parse(input, cont);
        right.parse(input, cont);
      };
  }
}
