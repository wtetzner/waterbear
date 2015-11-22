package org.bovinegenius.wombat.regex.ast;

import lombok.Value;

@Value
public class RegexPlus implements RegexNode {
  private static final RegexPlus value = new RegexPlus();

  RegexNodeType type = RegexNodeType.Plus;

  private RegexPlus() {}

  public RegexPlus of() {
    return value;
  }
}
