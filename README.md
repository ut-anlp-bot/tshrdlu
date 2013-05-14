# tshrdlu
=======

Authors: **Jason Baldridge** (jasonbaldridge@gmail.com), Nazneen Rajani, Nick Wilson

This is a repository for [project](https://github.com/utcompling/applied-nlp/wiki/Course-Project) related code for [Applied NLP course](https://github.com/utcompling/applied-nlp/wiki) being taught by [Jason Baldridge](http://www.jasonbaldridge.com) at [UT Austin](http://www.utexas.edu). It implements a Twitter bot original written by Jason Baldridge, extended by students in the course, and then customized by Nazneen and Nick for their course project.

The name "tshrdlu" comes from Twitter+[SHRDLU](http://en.wikipedia.org/wiki/SHRDLU).

## Requirements

* Version 1.6 of the Java 2 SDK (http://java.sun.com)

## Configuring your environment variables

The easiest thing to do is to set the environment variables `JAVA_HOME`
and `TSHRDLU_DIR` to the relevant locations on your system. Set `JAVA_HOME`
to match the top level directory containing the Java installation you
want to use.

Next, add the directory `TSHRDLU_DIR/bin` to your path. For example, you
can set the path in your `.bashrc` file as follows:

	export PATH=$PATH:$TSHRDLU_DIR/bin


Some functionality depends on [GeoNames](http://www.geonames.org/) API access
(free to sign up and use). You must create an account and set the environment
variable `TSHRDLU_GEONAMES_USERNAME` to your GeoNames username.

If you plan to index and search objects using the provided code based
on Lucene, you can customize the directory where on-disk indexes are
stored (the default is the tempdir, check the directory `tshrdlu`) by
setting the environment variable `TSHRDLU_INDEX_DIR`.

Once you have taken care of these things, you should be able to build and use
tshrdlu.

## Building the system from source

tshrdlu uses SBT (Simple Build Tool) with a standard directory
structure.  To build tshrdlu, type (in the `TSHRDLU_DIR` directory):

	$ ./build update compile

This will compile the source files and put them in
`./target/classes`. If this is your first time running it, you will see
messages about Scala being downloaded -- this is fine and
expected. Once that is over, the tshrdlu code will be compiled.

To try out other build targets, do:

	$ ./build

This will drop you into the SBT interface. To see the actions that are
possible, hit the TAB key. (In general, you can do auto-completion on
any command prefix in SBT, hurrah!)

To make sure all the tests pass, do:

	$ ./build test

Documentation for SBT is at <http://www.scala-sbt.org/>

Note: if you have SBT already installed on your system, you can
also just call it directly with "sbt" in `TSHRDLU_DIR`.

## Setting it up

The bot must be configured with the authentication information for the Twitter
user. The easiest way to do this is to create a `twitter4j.properties` file at
the root of the source directory with the following contents:

	oauth.consumerKey=<FILL THIS IN>
	oauth.consumerSecret=<FILL THIS IN>
	oauth.accessToken=<FILL THIS IN>
	oauth.accessTokenSecret=<FILL THIS IN>

## Trying it out

Run the bot:

	$ tshrdlu bot

When you tweet at the bot, it attempts to determine your location, select a
Twitter trend based on your location, and then reply to you with a response
automatically generated based on your own tweet history and the trending topic.

To assist in testing, the bot accepts a command to set the screen name of any
Twitter user as the target for user customization. The generated reply will still
be sent to the user who tweets at the bot, but the reply will be customized
based on the location of the configured user. To set the user, sent a tweet
directed at the bot with text like so (replacing `billgates` with the
desired screen name):

	Set User: billgates
