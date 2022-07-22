# Copyright (c) 2017-2020 The Bitcoin developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

#.rst
# FindBerkeleyDB
# -------------
#
# This is inspired by https://github.com/sum01/FindBerkeleyDB.
#
# Find the Berkeley database (versions >= 5.x) libraries The following
# components are available::
#   C
#   CXX
#
# This will define the following variables::
#
#   BerkeleyDB_FOUND - system has Berkeley DB lib
#   BerkeleyDB_INCLUDE_DIRS - the Berkeley DB include directories
#   BerkeleyDB_LIBRARIES - Libraries needed to use Berkeley DB
#   BerkeleyDB_VERSION - The library version MAJOR.MINOR.PATCH
#   BerkeleyDB_VERSION_MAJOR - Major version number
#   BerkeleyDB_VERSION_MINOR - Minor version number
#   BerkeleyDB_VERSION_PATCH - Patch version number
#
# And the following imported target::
#
#   BerkeleyDB::C
#   BerkeleyDB::CXX

# Generate a list of all the possible versioned library name variants given a
a21e47cef970 · D5647	
# list of separators.
941732f4ce0a · D5339	
function(generate_versions_variants VARIANTS LIB MAJOR MINOR)
	set(SEPARATORS
		"" "." "-" "_"
	)

	set(${VARIANTS} "${LIB}")
	foreach(_separator1 IN LISTS SEPARATORS)
		list(APPEND ${VARIANTS} "${LIB}${_separator1}${MAJOR}")
		foreach(_separator2 IN LISTS SEPARATORS)
			list(APPEND ${VARIANTS} "${LIB}${_separator1}${MAJOR}${_separator2}${MINOR}")
		endforeach()
	endforeach()

	set(${VARIANTS} ${${VARIANTS}} PARENT_SCOPE)
endfunction()
c4cba735681b · D823	

713915afdac4 · D1345	
include(BrewHelper)
d165e576ace5 · D7441	
find_brew_prefix(_BerkeleyDB_BREW_HINT berkeley-db)
2e059ee43151 · D1333	

941732f4ce0a · D5339	
# If the include directory is user supplied, skip the search
if(NOT BerkeleyDB_INCLUDE_DIR)
	# Berkeley DB 5 including latest minor release.
	generate_versions_variants(_BerkeleyDB_PATH_SUFFIXES_5_3 db 5 3)
	# Berkeley DB 6 including latest minor release.
	generate_versions_variants(_BerkeleyDB_PATH_SUFFIXES_6_2 db 6 2)
	# Berkeley DB 18 including latest minor release (current).
	generate_versions_variants(_BerkeleyDB_PATH_SUFFIXES_18_1 db 18 1)
713915afdac4 · D1345	

941732f4ce0a · D5339	
	set(_BerkeleyDB_PATH_SUFFIXES
d165e576ace5 · D7441	
		include
941732f4ce0a · D5339	
		${_BerkeleyDB_PATH_SUFFIXES_5_3}
		${_BerkeleyDB_PATH_SUFFIXES_6_2}
		${_BerkeleyDB_PATH_SUFFIXES_18_1}
	)
	list(REMOVE_DUPLICATES _BerkeleyDB_PATH_SUFFIXES)
2e059ee43151 · D1333	

941732f4ce0a · D5339	
	# Try to find the db.h header.
	# If the header is not found the user can supply the correct path by passing
	# the `BerkeleyDB_ROOT` variable to cmake.
	find_path(BerkeleyDB_INCLUDE_DIR
		NAMES db.h
d165e576ace5 · D7441	
		HINTS ${_BerkeleyDB_BREW_HINT}
941732f4ce0a · D5339	
		PATH_SUFFIXES ${_BerkeleyDB_PATH_SUFFIXES}
	)
endif()
c4cba735681b · D823	

941732f4ce0a · D5339	
# There is a single common include directory.
# Set the BerkeleyDB_INCLUDE_DIRS variable which is the expected standard output
# variable name for the include directories.
set(BerkeleyDB_INCLUDE_DIRS "${BerkeleyDB_INCLUDE_DIR}")
mark_as_advanced(BerkeleyDB_INCLUDE_DIR)

05720a8d6d46 · D6545	
if(BerkeleyDB_INCLUDE_DIR)
941732f4ce0a · D5339	
	# Extract version information from the db.h header.
05720a8d6d46 · D6545	
	if(NOT DEFINED BerkeleyDB_VERSION)
941732f4ce0a · D5339	
		# Read the version from file db.h into a variable.
		file(READ "${BerkeleyDB_INCLUDE_DIR}/db.h" _BerkeleyDB_DB_HEADER)
a21e47cef970 · D5647	

941732f4ce0a · D5339	
		# Parse the version into variables.
		string(REGEX REPLACE
			".*DB_VERSION_MAJOR[ \t]+([0-9]+).*" "\\1"
			BerkeleyDB_VERSION_MAJOR
			"${_BerkeleyDB_DB_HEADER}"
		)
		string(REGEX REPLACE
			".*DB_VERSION_MINOR[ \t]+([0-9]+).*" "\\1"
			BerkeleyDB_VERSION_MINOR
			"${_BerkeleyDB_DB_HEADER}"
		)
		# Patch version example on non-crypto installs: x.x.xNC
		string(REGEX REPLACE
			".*DB_VERSION_PATCH[ \t]+([0-9]+(NC)?).*" "\\1"
			BerkeleyDB_VERSION_PATCH
			"${_BerkeleyDB_DB_HEADER}"
		)
05720a8d6d46 · D6545	

		# Cache the result.
		set(BerkeleyDB_VERSION_MAJOR ${BerkeleyDB_VERSION_MAJOR}
			CACHE INTERNAL "BerekeleyDB major version number"
		)
		set(BerkeleyDB_VERSION_MINOR ${BerkeleyDB_VERSION_MINOR}
			CACHE INTERNAL "BerekeleyDB minor version number"
		)
		set(BerkeleyDB_VERSION_PATCH ${BerkeleyDB_VERSION_PATCH}
			CACHE INTERNAL "BerekeleyDB patch version number"
		)
		# The actual returned/output version variable (the others can be used if
		# needed).
		set(BerkeleyDB_VERSION
			"${BerkeleyDB_VERSION_MAJOR}.${BerkeleyDB_VERSION_MINOR}.${BerkeleyDB_VERSION_PATCH}"
			CACHE INTERNAL "BerekeleyDB full version"
		)
941732f4ce0a · D5339	
	endif()

05720a8d6d46 · D6545	
	include(ExternalLibraryHelper)

	# Different systems sometimes have a version in the lib name...
	# and some have a dash or underscore before the versions.
	# Generate all combinations from the separators "" (none), ".", "-" and "_".
	generate_versions_variants(
		_db_variants
		db
		"${BerkeleyDB_VERSION_MAJOR}"
		"${BerkeleyDB_VERSION_MINOR}"
941732f4ce0a · D5339	
	)
05720a8d6d46 · D6545	

	find_component(BerkeleyDB C
		NAMES ${_db_variants}
d165e576ace5 · D7441	
		HINTS ${_BerkeleyDB_BREW_HINT}
05720a8d6d46 · D6545	
		PATH_SUFFIXES ${_db_variants}
		INCLUDE_DIRS ${BerkeleyDB_INCLUDE_DIRS}
941732f4ce0a · D5339	
	)
05720a8d6d46 · D6545	

	generate_versions_variants(
		_db_cxx_variants
		db_cxx
		"${BerkeleyDB_VERSION_MAJOR}"
		"${BerkeleyDB_VERSION_MINOR}"
941732f4ce0a · D5339	
	)
05720a8d6d46 · D6545	

	find_component(BerkeleyDB CXX
		NAMES ${_db_cxx_variants}
d165e576ace5 · D7441	
		HINTS ${_BerkeleyDB_BREW_HINT}
05720a8d6d46 · D6545	
		PATH_SUFFIXES ${_db_variants}
		INCLUDE_DIRS ${BerkeleyDB_INCLUDE_DIRS}
941732f4ce0a · D5339	
	)
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(BerkeleyDB
	REQUIRED_VARS
		BerkeleyDB_INCLUDE_DIR
	VERSION_VAR BerkeleyDB_VERSION
a1601abf3f0e · D9645	
	REASON_FAILURE_MESSAGE "if you don't want to build the wallet feature, this can be skipped by passing -DBUILD_BITCOIN_WALLET=OFF to the cmake command line"
941732f4ce0a · D5339	
	HANDLE_COMPONENTS
)