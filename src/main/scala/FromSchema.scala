/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */
import com.squareup.kotlinpoet._
import sangria.ast._

object FromSchema {

  case class Config ( scalarTypes: Map[String, String]
                    , annotationOptions: AnnOptions
                    )
  case class AnnOptions()

  def makeEnumType(enumDef: EnumTypeDefinition)(implicit config: Config): TypeSpec = {
    val ret = TypeSpec.enumBuilder(enumDef.name)
    for (elem <- enumDef.values) {
      ret.addEnumConstant(elem.name)
    }
    ret.build()
  }

  def makeObjType(typeDef: ObjectTypeDefinition)(implicit config: Config): TypeSpec = {
    val ret = TypeSpec.classBuilder(typeDef.name)
    val cons = FunSpec.constructorBuilder()
    for (is <- typeDef.interfaces) {
      val cn = new ClassName("", is.name)
      ret.addSuperinterface(cn, "")
    }
    for (field <- typeDef.fields) {
      cons.addParameter(field.name, makeType(field.fieldType))
    }
    ret
      .primaryConstructor(cons.build())
      //      .addModifiers(KModifier.DATA)
      .build()
  }

  val list = new ClassName("", "List")
  def makeType(inTy: Type): TypeName = {
    inTy match {
      case NamedType(name, _) => new ClassName("", name).asNullable()
      case NotNullType(ofType, _) => makeType(ofType).asNonNullable()
      case ListType(ofType, _) => ParameterizedTypeName.get(list, makeType(ofType))
    }
  }

}
