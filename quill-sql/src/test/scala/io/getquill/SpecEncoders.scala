package io.getquill

import io.getquill.context.sql.{TestDecoders, TestEncoders}
import io.getquill.generic.{DecodingType, GenericDecoder, GenericEncoder, GenericNullChecker}

trait ExtraEncoders extends TestEntities with TestEncoders with TestDecoders with Spec
