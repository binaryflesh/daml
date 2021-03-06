// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.daml.lf.codegen

import com.digitalasset.daml.lf.data.Ref.{Identifier, QualifiedName, SimpleString}
import com.digitalasset.daml.lf.data.{BackStack, ImmArray, Ref}
import com.digitalasset.daml.lf.iface.reader.{Interface, InterfaceType}
import com.digitalasset.daml.lf.iface.{DefDataType, Record, Variant}
import com.typesafe.scalalogging.StrictLogging

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

private[codegen] sealed trait Node {
  def children: Map[String, Node]
}

private[codegen] final case class Module(modules: Map[String, Module], types: Map[String, Type])
    extends Node {
  override def children: Map[String, Node] = modules ++ [Node] types
}

private[codegen] final case class Type(typ: InterfaceType, types: Map[String, Type]) extends Node {
  override def children: Map[String, Node] = types
}

private[codegen] final case class InterfaceTree(
    modules: Map[String, Module],
    interface: Interface) {

  def process(f: NodeWithContext => Future[Unit])(implicit ec: ExecutionContext): Future[Unit] = {
    bfs(Future.successful(())) {
      case (a, nodeWithContext) => a.zipWith(f(nodeWithContext))((_, _) => ())(ec)
    }
  }

  def bfs[A](z: A)(f: (A, NodeWithContext) => A): A = {
    val nodeWithLineages = mutable.Queue.empty[NodeWithContext]
    for ((name, module) <- modules) {
      nodeWithLineages += ModuleWithContext(interface, BackStack.empty, name, module)
    }
    @tailrec
    def go(result: A): A = {
      if (nodeWithLineages.isEmpty) {
        result
      } else {
        val nodeWithContext = nodeWithLineages.dequeue()
        nodeWithLineages ++= nodeWithContext.childrenLineages
        go(f(result, nodeWithContext))
      }
    }
    go(z)
  }
}

private[codegen] sealed trait NodeWithContext {
  def interface: Interface
  def lineage: ImmArray[(String, Node)]
  def modulesLineage: BackStack[(String, Module)]
  def name: String
  def node: Node
  def children: Map[String, Node]
  def childrenLineages: Iterable[NodeWithContext]
  def typesLineages: Iterable[TypeWithContext]

  final def packageId: SimpleString = interface.packageId
}

private[codegen] final case class ModuleWithContext(
    interface: Interface,
    modulesLineage: BackStack[(String, Module)],
    name: String,
    module: Module)
    extends NodeWithContext {
  override def node: Node = module
  override def children: Map[String, Node] = module.modules ++ [Node] module.types
  override def childrenLineages: Iterable[NodeWithContext] = {
    val newModulesLineage = modulesLineage :+ (name -> module)
    module.modules.map {
      case (childName, childModule) =>
        ModuleWithContext(interface, newModulesLineage, childName, childModule)
    } ++ [NodeWithContext, Iterable[NodeWithContext]] typesLineages
  }

  override def typesLineages: Iterable[TypeWithContext] = module.types.map {
    case (childName, childType) =>
      TypeWithContext(
        interface,
        modulesLineage :+ (name -> module),
        BackStack.empty,
        childName,
        childType)
  }
  override def lineage: ImmArray[(String, Node)] = (modulesLineage :+ (name -> module)).toImmArray
}

private[codegen] final case class TypeWithContext(
    interface: Interface,
    modulesLineage: BackStack[(String, Module)],
    typesLineage: BackStack[(String, Type)],
    name: String,
    `type`: Type)
    extends NodeWithContext {
  override def node: Node = `type`
  override def children: Map[String, Node] = `type`.types
  override def childrenLineages: Iterable[NodeWithContext] = typesLineages
  override def typesLineages: Iterable[TypeWithContext] = `type`.types.map {
    case (childName, childType) =>
      TypeWithContext(
        interface,
        modulesLineage,
        typesLineage :+ (name -> `type`),
        childName,
        childType)
  }
  override def lineage: ImmArray[(String, Node)] =
    modulesLineage.toImmArray.slowAppend[(String, Node)](typesLineage.toImmArray)

  /* The name of this in the module */
  def fullName: Ref.DottedName = Ref.DottedName(typesLineage.map(_._1).:+(name).toImmArray)

  def module: Ref.ModuleName = Ref.ModuleName(modulesLineage.map(_._1).toImmArray)

  def qualifiedName: QualifiedName = QualifiedName(module, fullName)

  def identifier: Identifier = Identifier(packageId, qualifiedName)
}

private[codegen] object InterfaceTree extends StrictLogging {

  def fromInterface(interface: Interface): InterfaceTree = {
    val builder = InterfaceTreeBuilder.fromPackageId(interface.packageId)
    interface.getTypeDecls.asScala.foreach {
      case (identifier, typ) => builder.insert(identifier, typ)
    }
    builder.build(interface)
  }

  def print(`package`: InterfaceTree): Unit = {
    def printNodeType(prefix: String, what: String): Unit = {
      logger.info(s"$prefix [$what]")
    }
    def printTree(offset: Int)(nameAndNode: (String, Node)): Unit = {
      printNodeType(
        s"${" " * offset}⤷ ${nameAndNode._1}",
        nameAndNode._2 match {
          case _: Module => "Module"
          case Type(InterfaceType.Normal(DefDataType(_, _: Record.FWT)), _) => "Record"
          case Type(InterfaceType.Normal(DefDataType(_, _: Variant.FWT)), _) => "Variant"
          case Type(_: InterfaceType.Template, _) => "Template"
        }
      )
      nameAndNode._2.children.foreach(printTree(offset + 2))
    }
    logger.info(s"Content of Package ${`package`.interface.packageId.underlyingString}")
    `package`.modules.foreach(printTree(2))
  }

  private sealed trait NodeBuilder

  private final class ModuleBuilder(
      modules: mutable.HashMap[String, ModuleBuilder],
      types: mutable.HashMap[String, TypeBuilder])
      extends NodeBuilder {
    def build(): Module =
      Module(modules.mapValues(_.build()).toMap, types.mapValues(_.build()).toMap)
    @tailrec
    def insert(module: ImmArray[String], name: ImmArray[String], `type`: InterfaceType): Unit = {
      if (module.isEmpty) {
        // at this point name cannot be empty
        assert(name.length > 0)
        if (name.length == 1) {
          types.getOrElseUpdate(name.head, TypeBuilder.fromType(`type`)).setTypeOrThrow(`type`)
        } else {
          val tail = name.tail
          types.getOrElseUpdate(name.head, TypeBuilder.empty).insert(tail.head, tail.tail, `type`)
        }
      } else {
        modules.getOrElseUpdate(module.head, ModuleBuilder.empty).insert(module.tail, name, `type`)
      }
    }
  }

  private object ModuleBuilder {
    def empty = new ModuleBuilder(mutable.HashMap.empty, mutable.HashMap.empty)
  }

  private final class TypeBuilder(
      var typ: Option[InterfaceType],
      children: mutable.HashMap[String, TypeBuilder])
      extends NodeBuilder {
    def build(): Type = {
      typ match {
        case None =>
          throw new IllegalStateException(s"Found a Type node without a type at build() time")
        case Some(definedType) => Type(definedType, children.mapValues(_.build()).toMap)
      }
    }
    @tailrec
    def insert(name: String, names: ImmArray[String], `type`: InterfaceType): Unit = {
      if (names.isEmpty) {
        children
          .getOrElseUpdate(name, new TypeBuilder(Some(`type`), mutable.HashMap.empty))
          .setTypeOrThrow(`type`)
      } else {
        children
          .getOrElseUpdate(name, new TypeBuilder(None, mutable.HashMap.empty))
          .insert(names.head, names.tail, `type`)
      }
    }

    def setTypeOrThrow(typ: InterfaceType): Unit = {
      this.typ match {
        case Some(otherTyp) if typ != otherTyp =>
          throw new IllegalStateException(
            s"Found a Type node with two different types, $typ and $otherTyp. This should not happen")
        case _ => this.typ = Some(typ)
      }
    }
  }

  private object TypeBuilder {
    def empty = new TypeBuilder(None, mutable.HashMap.empty)
    def fromType(`type`: InterfaceType) = new TypeBuilder(Some(`type`), mutable.HashMap.empty)
  }

  private final class InterfaceTreeBuilder(
      val name: SimpleString,
      children: mutable.HashMap[String, ModuleBuilder]) {

    def build(interface: Interface): InterfaceTree =
      InterfaceTree(children.mapValues(_.build()).toMap, interface)

    def insert(qualifiedName: QualifiedName, `type`: InterfaceType): Unit = {
      children
        .getOrElseUpdate(qualifiedName.module.segments.head, ModuleBuilder.empty)
        .insert(qualifiedName.module.segments.tail, qualifiedName.name.segments, `type`)
    }
  }

  private object InterfaceTreeBuilder {
    def fromPackageId(packageId: SimpleString) =
      new InterfaceTreeBuilder(packageId, new mutable.HashMap())
  }
}

private[codegen] final case class InterfaceTrees(interfaceTrees: List[InterfaceTree])

private[codegen] object InterfaceTrees extends StrictLogging {

  def fromInterfaces(interfaces: Seq[Interface]): InterfaceTrees =
    InterfaceTrees(interfaces.map(InterfaceTree.fromInterface)(collection.breakOut))

  def print(interfaceTrees: InterfaceTrees): Unit =
    interfaceTrees.interfaceTrees.foreach(InterfaceTree.print)
}
