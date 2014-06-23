jsonpointer (v1.0.0)
====================

a tiny library to convert from json pointer syntax to lists of erlang terms and vice versa

see [rfc6901](http://tools.ietf.org/html/rfc6901) for
details of the specification

```erlang
1> jsonpointer:decode(<<"/foo/bar/baz">>).
[<<"foo">>,<<"bar">>,<<"baz">>]
2> jsonpointer:decode(<<"/foo/1/bar">>).
[<<"foo">>,<<"1">>,<<"bar">>]
3> jsonpointer:encode([<<"foo">>,<<"bar">>,<<"baz">>]).
<<"/foo/bar/baz">>
4> jsonpointer:encode([<<"foo">>,<<"1">>,<<"bar">>]).
<<"/foo/1/baz">>
5> jsonpointer:encode([<<"foo">>, 0, 1, 2]).
<<"/foo/0/1/2">>
6> jsonpointer:encode([foo, bar]).
<<"/foo/bar">>
7> jsonpointer:ref_to_int(<<"0">>).
0
8> jsonpointer:ref_to_int(<<"123456789">>).
123456789
```


