package io.getquill.context.jdbc

import java.sql.Types

trait BooleanIntEncoding {
  this: JdbcContextTypes[_, _] =>

  implicit val booleanEncoder: Encoder[Boolean] = encoder(Types.TINYINT, (index, value, row) => row.setInt(index, if (value) 1 else 0))
  implicit val booleanDecoder: Decoder[Boolean] = decoder((index, row, session) => row.getInt(index) == 1)
}
