package org.bovinegenius.wombat.regex.ast;

import lombok.Value;

@Value(staticConstructor="of")
public class RegexCharacter implements RegexNode {
  RegexNodeType type = RegexNodeType.Character;
  int character;
}
