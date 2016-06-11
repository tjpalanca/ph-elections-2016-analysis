PH Elections 2016 Analysis
================
<contact@tjpalanca.com>
June 11 2016

[![GitHub release](https://img.shields.io/github/release/tjpalanca/ph-elections-2016-analysis.svg?maxAge=2592000)](https://github.com/tjpalanca/fast-food-wars/) [![GitHub issues](https://img.shields.io/github/issues/tjpalanca/ph-elections-2016-analysis.svg?maxAge=2592000)](https://github.com/tjpalanca/fast-food-wars/issues)

------------------------------------------------------------------------

### Summary

A study of the Philippines 2016 elections.

------------------------------------------------------------------------

#### Part 1 - Election Fingerprints

Taking from the methodology rolled out by Klimek, Yegorov, Hanel, Thurner (2012)[1], we examine city/municipality level data to determine whether we can detect electoral fraud through vote padding by examining voter turnouts against share of the winning candidate. If fake votes are added to the count, some areas would exhibit an unusually high voter turnout, with a large number of votes going to the candidate favored by the fraud.

We can examine many candidates and races at once by taking a look at the distribution of the logarithmically scaled vote count (defined the detailed post) and flagging candidates that stray too far away from normality.

We also constructed a shiny widget to explore the results for all national elective positions.

**The complete analysis is laid out in this [blog post](http://tjpalanca.com/2016/05/on-elections-part-1.html).**

------------------------------------------------------------------------

### Publication

The results of this analysis will be published in [tjpalanca.com](http://www.tjpalanca.com), a data blog:

-   [Part 1 - Election Fingerprints](http://tjpalanca.com/2016/05/on-elections-part-1.html) - statistical electoral fraud detection via voter turnout and votes for winning candidate. [(GMA News Cross Post)](http://www.gmanetwork.com/news/story/568132/scitech/science/on-the-elections-part-1-election-fingerprints?utm_source=GMANews&utm_medium=Facebook&utm_campaign=GMANewsFacebook)

------------------------------------------------------------------------

### Project Information

#### Licensing

All rights reserved, except for those explicitly required to be provided by the GitHub Terms of Use. No part of this content may be reproduced, distributed, or transmitted in any form or by any means, without my prior explicit permission, except in the case of brief quotations, social media, and certain other noncommercial uses permitted by copyright law. You may not modify or otherwise benefit from the use of this content without my prior explicit permission.

#### Permission Requests

For permission requests, write to me at <contact@tjpalanca.com> with subject "Permission Request".

If you would like to use any content for your own work, I will give permission under the following conditions:

-   Please contact me first through the <contact@tjpalanca.com>.
-   Unless explicit written or e-mail permission has been provided, you may not use any content.
-   This content may not be used for commercial purposes, or for submission in academic work without citation.
-   This content may not be redistributed to others.
-   I retain the rights to use this content for any purpose.
-   I would appreciate a copy of the paper or a link to your work.
-   No warranties are provided.

#### Author Information

**Troy James R Palanca**

-   [Email](contact@tjpalanca.com)
-   [Blog](http://www.tjpalanca.com)
-   [Facebook Page](http://www.facebook.com/tjpalanca.blog)
-   [LinkedIn Profile](http://ph.linkedin.com/in/tjpalanca)

------------------------------------------------------------------------

[1] Klimek, Yegorov, Hanel, Thurner (2012). Statistical detection of systematic election irregularities. Proceedings of the National Academy of Sciences of the United States of America 109(41).
