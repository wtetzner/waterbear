package org.bovinegenius.wombat.gll.parser;

import java.util.HashSet;
import java.util.Set;
import java.util.Stack;

import lombok.Value;

@Value(staticConstructor="of")
public class Parser {
  CharSequence input;
  int current_index;
  Stack<Descriptor> R = new Stack<>();

  private static enum Label {
    L_0,
    L_S,
    L_S_0,
    L_S_1,
    L_S_2
  }

  private static enum Result {
    SUCCESS,
    FAILURE
  }

  @Value(staticConstructor="of")
  private static class Descriptor {
    Label label;
    
  }
  
  @Value(staticConstructor="of")
  private static class GssNode {
    Label label;
    int index;
  }
  
  private GssNode create(Label L, GssNode u, int i) {
    return null;
  }

  private GssNode add(Label L, Set<Label> u, int i) {
    return null;
  }

  private Result parse() {
    return parse(Label.L_S);
  }

  private Result parse(Label start) {
    add(start,new HashSet<Label>(),0);
    Label l = Label.L_0;
    while (true) {
      switch (l) {
        case L_0: {
          if (!R.isEmpty()) {
            Descriptor desc = R.pop();
            l = desc.getLabel();
          } else if (current_index >= input.length()) {
            return Result.SUCCESS;
          } else {
            return Result.FAILURE;
          }
        }
        case L_S: {
          
        }
      }
    }
  }
}



