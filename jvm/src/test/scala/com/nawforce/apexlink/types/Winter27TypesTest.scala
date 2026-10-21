/*
 Copyright (c) 2026 Kevin Jones, All rights reserved.
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.
 */

package com.nawforce.apexlink.types

import com.nawforce.apexlink.TestHelper
import org.scalatest.funsuite.AnyFunSuite

class Winter27TypesTest extends AnyFunSuite with TestHelper {

  test("Data 360 Limits methods") {
    happyTypeDeclaration("""public class Dummy {
        |  {
        |    Integer a = Limits.getDataCloudCursors();
        |    Integer b = Limits.getLimitDataCloudCursors();
        |    Integer c = System.Limits.getDataCloudCursorRows();
        |    Integer d = Limits.getLimitDataCloudCursorRows();
        |    Integer e = Limits.getDataCloudFlexCredits();
        |    Integer f = Limits.getLimitDataCloudFlexCredits();
        |  }
        |}""".stripMargin)
  }

  test("Data 360 Limits method with argument") {
    typeDeclaration("public class Dummy { { Integer a = Limits.getDataCloudCursors(1); } }")
    assert(
      dummyIssues ==
        "Missing: line 1 at 35-64: No matching method found for 'getDataCloudCursors' on 'System.Limits' taking arguments 'System.Integer', did you mean to call 'public static System.Integer getDataCloudCursors()'?\n"
    )
  }

  test("QueryOptions builder") {
    happyTypeDeclaration("""public class Dummy {
        |  {
        |    Database.QueryOptions opts = Database.QueryOptions.builder()
        |      .withDataspace('default')
        |      .withExplicitNamespace(true)
        |      .withHonorEmptyStrings(false)
        |      .build();
        |    String a = opts.getDataspace();
        |    Boolean b = opts.getExplicitNamespace();
        |    Boolean c = opts.getHonorEmptyStrings();
        |  }
        |}""".stripMargin)
  }

  test("QueryOptions constructor not visible") {
    typeDeclaration(
      "public class Dummy { { Database.QueryOptions opts = new Database.QueryOptions(); } }"
    )
    assert(
      dummyIssues ==
        "Error: line 1 at 77-79: Type cannot be constructed: Database.QueryOptions\n"
    )
  }

  test("QueryOptionsBuilder constructor not visible") {
    typeDeclaration(
      "public class Dummy { { Database.QueryOptionsBuilder b = new Database.QueryOptionsBuilder(); } }"
    )
    assert(
      dummyIssues ==
        "Error: line 1 at 88-90: Type cannot be constructed: Database.QueryOptionsBuilder\n"
    )
  }

  test("QueryOptionsBuilder wrong argument type") {
    typeDeclaration(
      "public class Dummy { { Database.QueryOptions.builder().withExplicitNamespace('x'); } }"
    )
    assert(
      dummyIssues ==
        "Missing: line 1 at 23-81: No matching method found for 'withExplicitNamespace' on 'Database.QueryOptionsBuilder' taking arguments 'System.String', wrong argument types for calling 'public virtual Database.QueryOptionsBuilder withExplicitNamespace(System.Boolean bool)'\n"
    )
  }

  test("CMS ConnectApi methods") {
    happyTypeDeclaration("""public class Dummy {
        |  {
        |    ConnectApi.ManagedContentJob job = ConnectApi.ManagedContent.getContentJob('id');
        |    ConnectApi.ManagedContentJobStatus status = job.status;
        |    ConnectApi.ManagedContentItemsDeleteOutput deleted =
        |      ConnectApi.ManagedContent.deleteContents('space', new ConnectApi.ManagedContentItemsDeleteInput());
        |    ConnectApi.ManagedContentDeliveryRendition r1 =
        |      ConnectApi.ManagedContentDelivery.getManagedContentRenditionForChannel('c', 'k', 'r');
        |    ConnectApi.ManagedContentDeliveryRendition r2 =
        |      ConnectApi.ManagedContentDelivery.getManagedContentRenditionForChannel('c', 'k', 'r', 'en_US');
        |    ConnectApi.ManagedContentSpaceDeleteRepresentation space =
        |      ConnectApi.ManagedContentSpaces.deleteManagedContentSpace('space');
        |  }
        |}""".stripMargin)
  }

  test("ObjectMilestoneCreatedEvent details") {
    happyTypeDeclaration("""public class Dummy {
        |  {
        |    ObjectMilestoneCreatedEvent e = new ObjectMilestoneCreatedEvent();
        |    List<ObjectMilestoneCreatedDtlEvent> details = e.ObjectMilestoneDetails;
        |    Id milestoneId = details[0].ObjectMilestoneId;
        |    Id slaId = e.SlaProcessId;
        |  }
        |}""".stripMargin)
  }

  test("ChangeEventHeader field and method access") {
    happyTypeDeclaration("""public class Dummy {
        |  {
        |    AccountChangeEvent e = new AccountChangeEvent();
        |    EventBus.ChangeEventHeader header = e.ChangeEventHeader;
        |    List<String> fields = e.ChangeEventHeader.getChangedFields();
        |    String changeType = e.ChangeEventHeader.changeType;
        |    Long commitNumber = e.ChangeEventHeader.getCommitNumber();
        |    Boolean last = e.ChangeEventHeader.isTransactionEnd();
        |  }
        |}""".stripMargin)
  }

  test("ChangeEventHeader is typed") {
    typeDeclaration(
      "public class Dummy { { Integer x = new AccountChangeEvent().ChangeEventHeader; } }"
    )
    assert(
      dummyIssues ==
        "Error: line 1 at 31-77: Incompatible types in assignment, from 'eventbus.ChangeEventHeader' to 'System.Integer'\n"
    )
  }

  test("ChangeEventHeader unknown field") {
    typeDeclaration(
      "public class Dummy { { String x = new AccountChangeEvent().ChangeEventHeader.foo; } }"
    )
    assert(
      dummyIssues ==
        "Missing: line 1 at 34-80: Unknown field or type 'foo' on 'eventbus.ChangeEventHeader'\n"
    )
  }
}
