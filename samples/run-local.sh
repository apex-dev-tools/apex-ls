#!/bin/bash
set -x
set -e
set -o pipefail

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/TriggerX/TriggerX

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/apex-query-builder/apex-query-builder

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/salesforce-bot-toolkit/salesforce-bot-toolkit

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/SalesforceDurableStreamingDemo/SalesforceDurableStreamingDemo

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/amoss/amoss

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/SimpleLightningComponents/SimpleLightningComponents

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/FormulaShare-DX/FormulaShare-DX

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/Apex-for-Xero/Apex-for-Xero

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/sendgrid-apex/sendgrid-apex

# TODO: This fails when run locally, not clear why
# java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/FindNearby

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/SFDCRules/SFDCRules

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/forcelog/forcelog

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/Apex-XML-Serializer/Apex-XML-Serializer

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/ObjectMerge/ObjectMerge

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/grid/grid

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/soql-secure/soql-secure

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/box-salesforce-sdk/box-salesforce-sdk

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/Angular/Angular

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/Lightweight-Trigger-Framework/Lightweight-Trigger-Framework

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/SObjectFabricator/SObjectFabricator

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/ApexTriggerHandler/ApexTriggerHandler

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/forcedotcom-enterprise-architecture/forcedotcom-enterprise-architecture

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/sf-sandbox-post-copy/sf-sandbox-post-copy

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/Multi-File-Uploader-Force/Multi-File-Uploader-Force

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/logger/logger

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/apex-sobjectdataloader/apex-sobjectdataloader

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/q/q

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/MyTriggers/MyTriggers

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/ForceDotComSprintWall/ForceDotComSprintWall

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/apex-rest-route/apex-rest-route

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/flowtoolbelt/flowtoolbelt

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/user-access-visualization/user-access-visualization

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/einstein-ai/einstein-ai

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/purealoe-lwc

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/HyperBatch

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/Interactions-for-Student-Recruitment

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/sobject-work-queue/sobject-work-queue

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/ffhttp-core/ffhttp-core

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/Force.com-Toolkit-for-Facebook/Force.com-Toolkit-for-Facebook

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/sfdc-oauth-playground/sfdc-oauth-playground

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/salesforce-limit-monitor/salesforce-limit-monitor

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/Centralized-Salesforce-Dev-Framework/Centralized-Salesforce-Dev-Framework

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/eventlogging/eventlogging

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/quiz-host-app/quiz-host-app

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/sfdc-related-files-lightning/sfdc-related-files-lightning

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/sObject-Remote/sObject-Remote

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/apex-domainbuilder/apex-domainbuilder

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/Force.com-Helper-Classes/Force.com-Helper-Classes

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/selector/selector

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/R-apex/R-apex

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/apex-dml-manager/apex-dml-manager

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/ConnectApiHelper/ConnectApiHelper

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/sfdc-convert-attachments-to-chatter-files/sfdc-convert-attachments-to-chatter-files

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/Forceea-data-factory/Forceea-data-factory/dx

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/ApexTestKit/ApexTestKit

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/jsonparse/jsonparse

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/sirono-common/sirono-common

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/apex-trigger-actions-framework/apex-trigger-actions-framework

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/esapi/esapi

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/EnhancedLightningGrid/EnhancedLightningGrid

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/processBuilderBlocks/processBuilderBlocks

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/promise/promise

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/Query.apex/Query.apex

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/apex-unified-logging/apex-unified-logging

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/Zippex/Zippex

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/TestDataFactory/TestDataFactory

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/automation-components/automation-components

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/force-di/force-di

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/EDA/EDA

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/apex-lambda/apex-lambda

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/sfdx-mass-action-scheduler/sfdx-mass-action-scheduler

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/apex-recipes/apex-recipes

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/apex-mdapi/apex-mdapi

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/apex-toolingapi/apex-toolingapi

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/Automated-Testing-for-Force/Automated-Testing-for-Force

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/CustomMetadataLoader/CustomMetadataLoader

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/declarative-lookup-rollup-summaries/declarative-lookup-rollup-summaries

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/df12-apex-enterprise-patterns/df12-apex-enterprise-patterns

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/df12-deployment-tools/df12-deployment-tools

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/dreamhouse-sfdx/dreamhouse-sfdx

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/fflib-apex-mocks/fflib-apex-mocks

cp -r samples/fflib-apex-mocks/fflib-apex-mocks samples/fflib-apex-common
java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/fflib-apex-common

cp -r samples/fflib-apex-mocks/fflib-apex-mocks samples/fflib-apex-common-samplecode
cp -r samples/fflib-apex-common/fflib-apex-common samples/fflib-apex-common-samplecode
java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/fflib-apex-common-samplecode

cp -r samples/fflib-apex-mocks/fflib-apex-mocks samples/at4dx
cp -r samples/fflib-apex-common/fflib-apex-common samples/at4dx
cp -r samples/force-di/force-di samples/at4dx
java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/at4dx

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/HEDAP/HEDAP

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/Milestones-PM/Milestones-PM

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/Salesforce-Lookup-Rollup-Summaries/Salesforce-Lookup-Rollup-Summaries

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/Salesforce-Test-Factory/Salesforce-Test-Factory

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/sfdc-trigger-framework/sfdc-trigger-framework

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/SmartFactory-for-Force/SmartFactory-for-Force

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/twilio-salesforce/twilio-salesforce

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/Visualforce-Multiselect-Picklist/Visualforce-Multiselect-Picklist

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/visualforce-table-grid/visualforce-table-grid

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/visualforce-typeahead/visualforce-typeahead

java -cp "apexlink/target/dependency/*:apexlink/target/apexlink.jar" com.nawforce.apexlink.ApexLink -nocache -outlinemulti samples/Cumulus

