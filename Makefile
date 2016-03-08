SBT		?= sbt
SBT_FLAGS	?= -Dsbt.log.noformat=true
RM_DIRS 	:= test-outputs test-reports
CLEAN_DIRS	:=

SRC_DIR	?= .
TEST_OUTPUT_DIR ?= ./test-outputs

.PHONY:	smoke publish-local check clean jenkins-build coverage scaladoc test

default:	publish-local

smoke:
	$(SBT) $(SBT_FLAGS) compile

publish-local:
	$(SBT) $(SBT_FLAGS) publish-local

check test:
	$(SBT) $(SBT_FLAGS) test

coverage:
	$(SBT) $(SBT_FLAGS) coverage test
	$(SBT) $(SBT_FLAGS) coverageReport

clean:
	$(SBT) $(SBT_FLAGS) +clean
	for dir in $(CLEAN_DIRS); do $(MAKE) -C $$dir clean; done
	$(RM) -r $(RM_DIRS)

scaladoc:
	$(SBT) $(SBT_FLAGS) doc test:doc

# Start off clean, then run tests for all supported configurations, and publish those versions of the code.
# Then run coverage and style tests (for developer's use).
# Don't publish the coverage test code since it contains hooks/references to the coverage test package
# and we don't want code with those dependencies published.
# We need to run the coverage tests last, since Jenkins will fail the build if it can't find their results.
jenkins-build: clean
	$(SBT) $(SBT_FLAGS) +test
	$(SBT) $(SBT_FLAGS) +clean +publish-local
	$(SBT) $(SBT_FLAGS) scalastyle coverage test
	$(SBT) $(SBT_FLAGS) coverageReport
