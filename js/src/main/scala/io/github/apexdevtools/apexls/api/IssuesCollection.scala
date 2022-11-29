/*
 Copyright (c) 2021 Kevin Jones, All rights reserved.
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

package io.github.apexdevtools.apexls.api

/** Access to Issues (an error or warning) for metadata files. When metadata files are changed
  * issues my reported against different files than the one changed. To make it easier to refresh
  * the collection tracks which files have changed issues since they were last retrieved.
  */
trait IssuesCollection {

  /** Array of metadata file paths whose issues have changed since they were last retrieved from
    * this collection.
    */
  def hasUpdatedIssues: Array[String]

  /** Reset the issue update handling for a metadata file. This will cause the file not to be
    * returned from the next call to hasUpdatedIssues() unless new issues are reported.
    */
  def ignoreUpdatedIssues(path: String): Unit

  /** Array of current Issues for the metadata file path. Returns an empty array if there are none.
    * Resets the updated issues tracking when called.
    */
  def issuesForFile(path: String): Array[Issue]

  /** Array of current Issues for the metadata file path that are entirely enclosed within a code
    * range defined by the Location. Returns an empty array if there are none. This does not reset
    * the updates issues tracking.
    */
  def issuesForFileLocation(path: String, location: IssueLocation): Array[Issue]

  /** Array of current Issues for multiple metadata files. If paths is null then this will return
    * issues for all metadata files which have them. When includeWarnings is true, issues of any
    * category will be returned otherwise only errors will be returned. The argument
    * maxErrorsPerFile limits the number of Issues returned to the first 'n' on each file, if it is
    * <1 all Issues will be returned for each metadata file. Resets the updated issues tracking when
    * called for all files processed.
    */
  def issuesForFiles(
    paths: Array[String],
    includeWarnings: Boolean,
    maxErrorsPerFile: Int
  ): Array[Issue]
}
