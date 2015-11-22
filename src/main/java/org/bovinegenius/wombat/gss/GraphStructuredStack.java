package org.bovinegenius.wombat.gss;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

import lombok.Getter;
import lombok.NonNull;
import lombok.Value;

public class GraphStructuredStack<T> {
  @Getter private final List<Node<T>> top;
  private final Map<T,T> seen;

  private GraphStructuredStack() {
    this.top = new ArrayList<>();
    this.seen = new HashMap<T,T>();
  }

  
  @Value(staticConstructor="of")
  public static class Node<T> {
    T data;
    Optional<Node<T>> previous;
  }
}
