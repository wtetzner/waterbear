package org.bovinegenius.wombat.regex.ast;

import lombok.Value;

@Value(staticConstructor="of")
public class RegexCaptureGroup implements RegexNode {
  RegexNodeType type = RegexNodeType.CaptureGroup;
  RegexNode body;
}
