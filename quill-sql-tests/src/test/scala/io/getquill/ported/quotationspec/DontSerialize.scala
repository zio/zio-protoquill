package io.getquill.ported.quotationspec

import io.getquill.parser.DoSerialize
import io.getquill.parser.SerializationBehavior

// Will disable AST serialization for quotes in packages/subpackages with this inside.
given DoSerialize with
  override type BehaviorType = SerializationBehavior.Serialize