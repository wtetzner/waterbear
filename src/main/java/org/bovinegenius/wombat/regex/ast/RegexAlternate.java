package org.bovinegenius.wombat.regex.ast;

import lombok.Value;

@Value(staticConstructor="of")
public class RegexAlternate implements RegexNode {
  RegexNodeType type = RegexNodeType.Alternate;
  RegexNode left;
  RegexNode right;
}
