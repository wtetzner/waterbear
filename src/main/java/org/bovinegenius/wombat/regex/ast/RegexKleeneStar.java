package org.bovinegenius.wombat.regex.ast;

import lombok.Value;

@Value
public class RegexKleeneStar implements RegexNode {
  private static final RegexKleeneStar value = new RegexKleeneStar();

  RegexNodeType type = RegexNodeType.KleeneStar;

  private RegexKleeneStar() {}

  public RegexKleeneStar of() {
    return value;
  }
}
