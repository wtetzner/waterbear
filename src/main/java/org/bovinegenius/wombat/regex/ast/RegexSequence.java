package org.bovinegenius.wombat.regex.ast;

import lombok.Value;

@Value(staticConstructor="of")
public class RegexSequence implements RegexNode {
  RegexNodeType type = RegexNodeType.Sequence;
  RegexNode left;
  RegexNode right;
}
