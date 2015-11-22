package org.bovinegenius.wombat.gll;

import lombok.EqualsAndHashCode;
import lombok.Value;

public abstract class Result<I,T,E> {
  public abstract boolean isSuccess();
  public abstract boolean isFailure();

  private Result() {}

  @Value(staticConstructor="of")
  @EqualsAndHashCode(callSuper=false)
  public static class Success<I,T,E> extends Result<I,T,E> {
    T value;
    I rest;

    @Override public boolean isSuccess() { return true; }
    @Override public boolean isFailure() { return false; }
  }

  @Value(staticConstructor="of")
  @EqualsAndHashCode(callSuper=false)
  public static class Failure<I,T,E> extends Result<I,T,E> {
    E error;
    I rest;

    @Override public boolean isSuccess() { return false; }
    @Override public boolean isFailure() { return true; }
  }
}
