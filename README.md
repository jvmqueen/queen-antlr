# queen-antlr

[![Managed By Self XDSD](https://self-xdsd.com/b/mbself.svg)](https://self-xdsd.com/p/jvmqueen/queen-antlr?provider=github)
[![DevOps By Rultor.com](http://www.rultor.com/b/jvmqueen/queen-antlr)](http://www.rultor.com/p/jvmqueen/queen-antlr)
[![We recommend IntelliJ IDEA](http://amihaiemil.github.io/images/intellij-idea-recommend.svg)](https://www.jetbrains.com/idea/)

[![Build Status](https://travis-ci.org/jvmqueen/queen-antlr.svg?branch=master)](https://travis-ci.org/jvmqueen/queen-antlr)

This repo contains [Queenlang](https://queenlang.org/)'s ANTLR4 grammar, as well as the Java 8 parser generated
by ANTLR4.

To get the latest release from Github Packages, simply add the following to your ``pom.xml``:

```xml
<dependency>
    <groupId>org.queenlang</groupId>
    <artifactId>queen-antlr</artifactId>
    <version>0.0.4</version>
</dependency>
```

## Contribute

Contributors are welcome!

1. Open an issue regarding an improvement you thought of, or a bug you noticed, or ask to be assigned to an existing one.
2. If the issue is confirmed, fork the repository, do the changes on a separate branch and make a Pull Request.
3. After review and acceptance, the PR is merged and closed.

Make sure the maven build

``$ mvn clean install``

**passes before making a PR**. 