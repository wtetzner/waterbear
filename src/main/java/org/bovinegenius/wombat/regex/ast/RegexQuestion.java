package org.bovinegenius.wombat.regex.ast;

import lombok.Value;

@Value
public class RegexQuestion implements RegexNode {
  private static final RegexQuestion value = new RegexQuestion();

  RegexNodeType type = RegexNodeType.Question;

  private RegexQuestion() {}

  public RegexQuestion of() {
    return value;
  }
}
