package org.bovinegenius.wombat.gll;

import java.util.function.Function;

public interface Parser<I,T,E> {
  public void parse(I input, Function<Result<I,T,E>,Void> continuation);
}
