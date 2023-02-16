window.BENCHMARK_DATA = {
  "lastUpdate": 1676509267207,
  "repoUrl": "https://github.com/ocaml/dune",
  "entries": {
    "Melange Benchmark": [
      {
        "commit": {
          "author": {
            "email": "javier.chavarri@gmail.com",
            "name": "Javier Chávarri",
            "username": "jchavarri"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "905247a2e69cbc0a10fd86a314504e4103eaf40c",
          "message": "melange: add build benchmark to ci (#6791)\n\nSigned-off-by: Javier Chávarri <javier.chavarri@gmail.com>",
          "timestamp": "2022-12-29T15:53:30-06:00",
          "tree_id": "06291f117daf78ff12d184f40b41ec0b3755bd2b",
          "url": "https://github.com/ocaml/dune/commit/905247a2e69cbc0a10fd86a314504e4103eaf40c"
        },
        "date": 1672352576731,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "41.88697116556667",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "752ba97135b04d1f5e4a4c171bd35f3551e73c65",
          "message": "test(melange): include_subdirs (#6810)\n\ntest should include .js paths to show that they're currently wrong\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2022-12-29T16:46:32-06:00",
          "tree_id": "08bc84c1c9e4ad86e6b817bde15acc4d309d9003",
          "url": "https://github.com/ocaml/dune/commit/752ba97135b04d1f5e4a4c171bd35f3551e73c65"
        },
        "date": 1672355228489,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "38.341008513940004",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "344f618d035ccb12fea78f4f8bb3996d0192fd3c",
          "message": "chore(fiber): add pool benchmarks (#6813)\n\nBenchmark the Fiber.Pool implementation\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2022-12-30T00:44:54-06:00",
          "tree_id": "7bbb1f43a7b7bb7a2ccea15b16cf5dff87145f82",
          "url": "https://github.com/ocaml/dune/commit/344f618d035ccb12fea78f4f8bb3996d0192fd3c"
        },
        "date": 1672384014418,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "40.397050457726664",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "cf96d82837dd422391bac47cdc747b098018ee65",
          "message": "test(fiber): Pool.{run,stop} tests (#6812)\n\n* double running a pool should be forbidden\r\n* stopping and then running is allowed\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2022-12-30T01:30:13-06:00",
          "tree_id": "8cab5ec95b4e1b7d2198aa94fe0f7b6dd28ca90a",
          "url": "https://github.com/ocaml/dune/commit/cf96d82837dd422391bac47cdc747b098018ee65"
        },
        "date": 1672390817074,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "38.03061777441334",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "b1ae0c773116e53a911d934cbdb9f6d5dbbe25ff",
          "message": "refactor(rpc): distinguish Timeout from Shutdown (#6802)\n\nWhen the scheduler shuts down due to a timeout (during testing), we\r\nclarify this in the error message.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2022-12-31T09:10:03-06:00",
          "tree_id": "42d5b03f36bae68c7f64240f6fff18c88fa53f27",
          "url": "https://github.com/ocaml/dune/commit/b1ae0c773116e53a911d934cbdb9f6d5dbbe25ff"
        },
        "date": 1672500975994,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "48.57778337701334",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "1b6570f7011cc21a53fa7bb3a5cd077aa84c69b2",
          "message": "fix(dune_console): print missing newline after dune exec (#6821)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-01-03T19:44:06-06:00",
          "tree_id": "b66f046a22726b54e56eb46d6beac2cae19a2f9d",
          "url": "https://github.com/ocaml/dune/commit/1b6570f7011cc21a53fa7bb3a5cd077aa84c69b2"
        },
        "date": 1672798529027,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "38.658632765246665",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "bf97454e2202098f3a08a13c8ccd1d0087f047a2",
          "message": "chore: update build_path_prefix_map (#6826)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-01-04T13:21:16-06:00",
          "tree_id": "12fd23fc50e0b89320b4f45b64e32238c5297f75",
          "url": "https://github.com/ocaml/dune/commit/bf97454e2202098f3a08a13c8ccd1d0087f047a2"
        },
        "date": 1672861360305,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "37.50016077142667",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "2609315+esope@users.noreply.github.com",
            "name": "Benoit Montagu",
            "username": "esope"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "9f6da11209cbb2e47cebf09e1002a35a2cd2be9d",
          "message": "Use alphabetical ordering of stanzas in manual (#6824)\n\nSigned-off-by: Benoît Montagu <benoit.montagu@inria.fr>",
          "timestamp": "2023-01-04T13:22:35-06:00",
          "tree_id": "df945556fc1edc9ab1ba2ce61a547c8fa979717a",
          "url": "https://github.com/ocaml/dune/commit/9f6da11209cbb2e47cebf09e1002a35a2cd2be9d"
        },
        "date": 1672861489801,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "41.87944795932",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "1489c57adc6483f3d98b9621e63b85bbd50cdc89",
          "message": "feature(cache): add `dune cache size` command (#6638)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-01-04T13:28:21-06:00",
          "tree_id": "4d73b6cbf9105b2f0234d97a7de2590b369b5db2",
          "url": "https://github.com/ocaml/dune/commit/1489c57adc6483f3d98b9621e63b85bbd50cdc89"
        },
        "date": 1672861600249,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.57483041076667",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "fa91afcbe1dab60d868df32d09b331a94fe30efb",
          "message": "chore(nix): update flakes (#6806)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-04T13:26:09-06:00",
          "tree_id": "56b220a06ea4597996e09c3bdf200e40216ad299",
          "url": "https://github.com/ocaml/dune/commit/fa91afcbe1dab60d868df32d09b331a94fe30efb"
        },
        "date": 1672861787155,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "46.08527931125334",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "cadeaudeelie@gmail.com",
            "name": "Et7f3",
            "username": "Et7f3"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "cde7139c8e25836f1b2d41819ba55d9e925fa332",
          "message": "build: needs CoreFoundation instead of Foundation (#6829)\n\nSigned-off-by: Élie BRAMI <cadeaudeelie@gmail.com>",
          "timestamp": "2023-01-04T15:54:21-06:00",
          "tree_id": "6e8396db40890952c92cba17c66ed72c7a561772",
          "url": "https://github.com/ocaml/dune/commit/cde7139c8e25836f1b2d41819ba55d9e925fa332"
        },
        "date": 1672870729900,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.38860668158667",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "n.oje.bar@gmail.com",
            "name": "Nicolás Ojeda Bär",
            "username": "nojb"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "620b98bb01835ac846dbd352c4b62c7d1bfcb697",
          "message": "Fix Jsoo rules bug: artifacts of libraries with public names are not found (#6828)\n\nSigned-off-by: Nicolás Ojeda Bär <n.oje.bar@gmail.com>\r\nSigned-off-by: Hugo Heuzard <hugo.heuzard@gmail.com>\r\nCo-authored-by: Hugo Heuzard <hugo.heuzard@gmail.com>",
          "timestamp": "2023-01-05T06:57:02+01:00",
          "tree_id": "1c3a91166e5e0b7ecdbb43b012528d4fa065d674",
          "url": "https://github.com/ocaml/dune/commit/620b98bb01835ac846dbd352c4b62c7d1bfcb697"
        },
        "date": 1672899688545,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "47.340776414353336",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "david.allsopp@metastack.com",
            "name": "David Allsopp",
            "username": "dra27"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "35d9a3c92bd874547f6b9d9cca6cfa4968ac87cd",
          "message": "Fix boot/libs.ml between 4.x/5.x (#6753)\n\nSigned-off-by: David Allsopp <david.allsopp@metastack.com>",
          "timestamp": "2023-01-05T09:39:21-06:00",
          "tree_id": "bed9658eb4715c6a395248f56646f37aea81c6c4",
          "url": "https://github.com/ocaml/dune/commit/35d9a3c92bd874547f6b9d9cca6cfa4968ac87cd"
        },
        "date": 1672934276015,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.273181545713335",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "4358616cc6cfc09f188a9a50051b1cae1db964a9",
          "message": "refactor(rules): move cram rules to own dir (#6835)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-05T15:50:54-06:00",
          "tree_id": "9516be3a59854bb743abb58fe11d8465cd75d157",
          "url": "https://github.com/ocaml/dune/commit/4358616cc6cfc09f188a9a50051b1cae1db964a9"
        },
        "date": 1672956778829,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "41.89532303838",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "f2c106256296af86c81301ffe95c926ef74c610c",
          "message": "refactor(rules): move ctypes to own dir (#6834)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-05T18:38:47-06:00",
          "tree_id": "2ddc0c11e870d42cc659eaf98acac8b205324562",
          "url": "https://github.com/ocaml/dune/commit/f2c106256296af86c81301ffe95c926ef74c610c"
        },
        "date": 1672966738189,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "37.11957626576",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "1c84d1b2b9035565c37cfba6ad51d4ff22c781f4",
          "message": "refactor: move ocaml commands to own folder (#6833)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-06T09:21:43-06:00",
          "tree_id": "a5c1da127e65f53d3267054cdc513a2a1a1537f0",
          "url": "https://github.com/ocaml/dune/commit/1c84d1b2b9035565c37cfba6ad51d4ff22c781f4"
        },
        "date": 1673020258265,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.83941521134667",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@emillon.org",
            "name": "Etienne Millon",
            "username": "emillon"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "63e6845a89eab969323c88673533271b9e3c0baf",
          "message": "Ctypes: detect duplicate function descriptions (#6751)\n\n* test: ctypes stanza with dup function_description\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>\r\n\r\n* fix: improve error message\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>",
          "timestamp": "2023-01-06T17:27:14+01:00",
          "tree_id": "20c0a4d76c9b3bbe15f97a143c8384a95541fa1b",
          "url": "https://github.com/ocaml/dune/commit/63e6845a89eab969323c88673533271b9e3c0baf"
        },
        "date": 1673024178955,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "44.05186140538666",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "0a6ba087f185cf898ec46f4488e4dcf4a90dda93",
          "message": "refactor: move module settings to a common type (#6819)\n\nModule settings are now stored in a single record. These settings are\r\nshared between melange stanzas, executables, and libraries.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-08T10:06:48-06:00",
          "tree_id": "04f3b0a5be69560f503aa18b27013202446521eb",
          "url": "https://github.com/ocaml/dune/commit/0a6ba087f185cf898ec46f4488e4dcf4a90dda93"
        },
        "date": 1673195324560,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "32.67942630764",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "anmonteiro@gmail.com",
            "name": "Antonio Nuno Monteiro",
            "username": "anmonteiro"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "df0ec33fc99e175f08fff6364c7c2a6ac6ed1e5a",
          "message": "chore(nix): remove `dune_3` from devShells.slim.buildInputs (#6840)\n\nthis makes `./dune.exe exec -- $EDITOR` work\r\n\r\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-01-09T09:50:02-06:00",
          "tree_id": "8f8ea91c46790ead384f904975dd45d839f04b0b",
          "url": "https://github.com/ocaml/dune/commit/df0ec33fc99e175f08fff6364c7c2a6ac6ed1e5a"
        },
        "date": 1673280535247,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.68372456580667",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "0167e6cf6d035268a38c8638f5d4cad7239580e3",
          "message": "fix(dyn): don't break boxes when printing (#6836)\n\nUse [Pp.cut] rather than [Pp.newline] as the seperator. This should\r\noutput a box respecting newline since we're in a vertical box.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-09T09:50:42-06:00",
          "tree_id": "788c9be6b7e86489b0951999d9a892eb5ec66b1c",
          "url": "https://github.com/ocaml/dune/commit/0167e6cf6d035268a38c8638f5d4cad7239580e3"
        },
        "date": 1673280658407,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "39.98567882435333",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@emillon.org",
            "name": "Etienne Millon",
            "username": "emillon"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "c2139223abdb31af83e5476baf249ef140a112b1",
          "message": "doc: split stanza documentation in several files (#6851)\n\nThe HTML output is identical (this is like a preprocessor include) but\r\nthis makes the source file a lot smaller.\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>",
          "timestamp": "2023-01-09T17:09:56+01:00",
          "tree_id": "4b91f8fbd5024c61c992543feb4a6dc054df423e",
          "url": "https://github.com/ocaml/dune/commit/c2139223abdb31af83e5476baf249ef140a112b1"
        },
        "date": 1673281711809,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.41149613973334",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "284fb0f1b393882c94be260c2898be5e2d531fd6",
          "message": "doc: add notes on benchmarks (#6815)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-01-09T10:19:29-06:00",
          "tree_id": "a76e0dcbdde70fcc4e2fa1cd7d9021d096782710",
          "url": "https://github.com/ocaml/dune/commit/284fb0f1b393882c94be260c2898be5e2d531fd6"
        },
        "date": 1673282300985,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.24856346404666",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@emillon.org",
            "name": "Etienne Millon",
            "username": "emillon"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "ebcad123c43868b7a5c1a9fefdbac5ec142afbbb",
          "message": "doc: remove spurious bold styling in ctypes doc (#6849)\n\nSigned-off-by: Etienne Millon <me@emillon.org>",
          "timestamp": "2023-01-09T17:17:18+01:00",
          "tree_id": "be1747dacb33e0d868e24eb80b58f5aaf58331e0",
          "url": "https://github.com/ocaml/dune/commit/ebcad123c43868b7a5c1a9fefdbac5ec142afbbb"
        },
        "date": 1673282445920,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "43.29923563302666",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "stephen@sherra.tt",
            "name": "Stephen Sherratt",
            "username": "gridbugs"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "5ff9a4fb0be075a26379b537677e15398113f14a",
          "message": "feature: Eager watch mode for exec (#6507)\n\nSigned-off-by: Stephen Sherratt <stephen@sherra.tt>",
          "timestamp": "2023-01-09T10:22:18-06:00",
          "tree_id": "5c04e65112f5f5c8d1f722891ef3d98ae4627aa4",
          "url": "https://github.com/ocaml/dune/commit/5ff9a4fb0be075a26379b537677e15398113f14a"
        },
        "date": 1673282717156,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "44.03702566735333",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "github@vvalter.com",
            "name": "Simon Rainer",
            "username": "Vvalter"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "d822fd7a9d9c854e2d67f703a28da402a2c019c9",
          "message": "Fix formatting problems in quick-start.rst (#6846)\n\nSigned-off-by: Simon Rainer <sr@mail25.de>\r\n\r\nSigned-off-by: Simon Rainer <sr@mail25.de>\r\nCo-authored-by: Etienne Millon <me@emillon.org>",
          "timestamp": "2023-01-09T17:34:02+01:00",
          "tree_id": "b1942b1f8d46a2ff651511e5b1d280f283ad6fe6",
          "url": "https://github.com/ocaml/dune/commit/d822fd7a9d9c854e2d67f703a28da402a2c019c9"
        },
        "date": 1673283174457,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.91652652354",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@emillon.org",
            "name": "Etienne Millon",
            "username": "emillon"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "90ea7339e3b00eec10ef12aa6b2abdbb8d8cc313",
          "message": "doc: remove migration page (#6850)\n\n* doc: remove migration page\r\n\r\nThis document has been useful to document the jbuilder -> dune migration\r\nbut it is now not useful anymore.\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>\r\n\r\n* Remove MIGRATION.md\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>\r\n\r\n* Add a history paragraph\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>",
          "timestamp": "2023-01-09T17:53:52+01:00",
          "tree_id": "1f13a45ac8a3a116ae7cca85fa3cbb28726a3614",
          "url": "https://github.com/ocaml/dune/commit/90ea7339e3b00eec10ef12aa6b2abdbb8d8cc313"
        },
        "date": 1673284360402,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.09777807080667",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "715825e0b032015ff7f1652a2b3ea448513c43ac",
          "message": "chore: move menhir rules to menhir dir (#6858)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-01-09T19:09:30-06:00",
          "tree_id": "b1cd254d3237431abca4360c03fd589981491145",
          "url": "https://github.com/ocaml/dune/commit/715825e0b032015ff7f1652a2b3ea448513c43ac"
        },
        "date": 1673314105453,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.54283112262001",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "77136fa2813caaccd1738ce7c91c9c04e5350665",
          "message": "chore: move generate_sites_module rules to generate_sites_module (#6860)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-01-09T20:21:33-06:00",
          "tree_id": "9488841f668376b4ad5e494c3e5e0dd815a37d4e",
          "url": "https://github.com/ocaml/dune/commit/77136fa2813caaccd1738ce7c91c9c04e5350665"
        },
        "date": 1673318417158,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.678875267340004",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@emillon.org",
            "name": "Etienne Millon",
            "username": "emillon"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "d1e0d363cc19207c0739c0ff0bda5bde642facdd",
          "message": "doc: use sphinx version metadata instead of titles (#6863)\n\nThis removes \"since x.y\" from titles, in particular this unclutters the\r\nTOC.\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>",
          "timestamp": "2023-01-10T15:19:30+01:00",
          "tree_id": "1bdc320933a937bc5e73565a480d5d1cd4ff125b",
          "url": "https://github.com/ocaml/dune/commit/d1e0d363cc19207c0739c0ff0bda5bde642facdd"
        },
        "date": 1673361499644,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.416830723273335",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "129383785c30f65691822a8b27e10e57bfd03bfa",
          "message": "chore: move merlin rules to merlin dir (#6857)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-01-10T09:05:53-06:00",
          "tree_id": "36ce51209f92c0f73c31697359f0a46818c6acf8",
          "url": "https://github.com/ocaml/dune/commit/129383785c30f65691822a8b27e10e57bfd03bfa"
        },
        "date": 1673364735809,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "46.6337287115",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@emillon.org",
            "name": "Etienne Millon",
            "username": "emillon"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "b3af9e7974ba9a95e61e47ccdd3469a81a77aa3a",
          "message": "test: ctypes and relative include paths (#6838)\n\nThis adds a test that shows that under `(using ctypes 0.2)`, local\r\nheaders need to be added in two different locations because rules are\r\nexecuted from different places. See #5325.\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>",
          "timestamp": "2023-01-10T17:45:57+01:00",
          "tree_id": "5b92b181fb18444c50793f6bd8907e0d8a910e85",
          "url": "https://github.com/ocaml/dune/commit/b3af9e7974ba9a95e61e47ccdd3469a81a77aa3a"
        },
        "date": 1673370605806,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "42.62877656914666",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "javier.chavarri@gmail.com",
            "name": "Javier Chávarri",
            "username": "jchavarri"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "6d503e71e96d68b268e86a36b12c165bd107b8e2",
          "message": "ctypes: rename stanza to field (#6862)\n\nSigned-off-by: Javier Chávarri <javier.chavarri@gmail.com>",
          "timestamp": "2023-01-10T13:18:25-06:00",
          "tree_id": "2d4dc53174828773a753e92e866956db7766ca98",
          "url": "https://github.com/ocaml/dune/commit/6d503e71e96d68b268e86a36b12c165bd107b8e2"
        },
        "date": 1673379461209,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "36.999742637353336",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "committer": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "acc93cf7dc6233516cf979001e47dfc736899e60",
          "message": "chore: move coq bin and rules to own \"coq\" directories\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>\n\n<!-- ps-id: 05f539af-7554-4b91-9083-9f7870126e3c -->",
          "timestamp": "2023-01-10T21:45:42+01:00",
          "tree_id": "4c687d5f374ff61ff316f5a2f27722703c0c777f",
          "url": "https://github.com/ocaml/dune/commit/acc93cf7dc6233516cf979001e47dfc736899e60"
        },
        "date": 1673384650036,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.03580306272",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "03dcc5d6dfef684623f94017daddf1fd93ee3737",
          "message": "fix: staged_pps should work (#6748)\n\nSetting sandboxing by default would break staged_pps because it would\r\ntransitively make various compilation commands to be sandboxed as well\r\nwhich is not supported.\r\n\r\nWe restore the old (no sandboxing) default to staged_pps\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-10T17:59:12-06:00",
          "tree_id": "1784b2683e631f7d9a274e8ed43e69c15defb971",
          "url": "https://github.com/ocaml/dune/commit/03dcc5d6dfef684623f94017daddf1fd93ee3737"
        },
        "date": 1673396488802,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.099764013026665",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "anmonteiro@gmail.com",
            "name": "Antonio Nuno Monteiro",
            "username": "anmonteiro"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "7222f67542e2ed5d0b11d1b50bc2c6f729cb5ba6",
          "message": "feat(melange): install melange libraries (#6602)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>\r\nSigned-off-by: Javier Chavarri <javier.chavarri@gmail.com>\r\nCo-authored-by: Javier Chavarri <javier.chavarri@gmail.com>",
          "timestamp": "2023-01-10T20:01:28-06:00",
          "tree_id": "57196e4dc76f307924f3a7b1a567b6ec1437ae53",
          "url": "https://github.com/ocaml/dune/commit/7222f67542e2ed5d0b11d1b50bc2c6f729cb5ba6"
        },
        "date": 1673403629417,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.777687409546665",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@emillon.org",
            "name": "Etienne Millon",
            "username": "emillon"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "a83e71c3d715722ee7e774b32225c2c29bd57664",
          "message": "doc: use a table to document opam sections (#6868)\n\nSigned-off-by: Etienne Millon <me@emillon.org>\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>",
          "timestamp": "2023-01-11T17:54:13+01:00",
          "tree_id": "de84305f8feeb7223481205ccc32a7c1c73becfd",
          "url": "https://github.com/ocaml/dune/commit/a83e71c3d715722ee7e774b32225c2c29bd57664"
        },
        "date": 1673457205168,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "37.321133905059995",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "stephen@sherra.tt",
            "name": "Stephen Sherratt",
            "username": "gridbugs"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "184809335a06fcca5597a3ff35eee5729d894207",
          "message": "Revert \"feature: Eager watch mode for exec (#6507)\" (#6867)\n\nThis reverts commit 5ff9a4fb0be075a26379b537677e15398113f14a.\r\n\r\nThis was causing occasional segfaults on macos when running `dune exec`\r\nso reverting this until we figure out what's causing that.\r\n\r\nSigned-off-by: Stephen Sherratt <stephen@sherra.tt>",
          "timestamp": "2023-01-11T19:48:16-06:00",
          "tree_id": "e0c260cd8a69340209101bef18eaa0254d4e6468",
          "url": "https://github.com/ocaml/dune/commit/184809335a06fcca5597a3ff35eee5729d894207"
        },
        "date": 1673489227222,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.00361290173334",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "2a8c5ded687869bf0a7dea802105caaae55f8de8",
          "message": "refactor(merlin): remove unnecessary read_memo (#6871)\n\nThe use is completely unnecessary here as we're inside the action\r\nbuilder monad anyway\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-12T12:56:55-06:00",
          "tree_id": "71c2d7c9db4b0e4b10b5f42f58d389daf47d7dfa",
          "url": "https://github.com/ocaml/dune/commit/2a8c5ded687869bf0a7dea802105caaae55f8de8"
        },
        "date": 1673550956458,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.80896740932666",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "c2833c6aa56108731487f3840ff72ed29cff374b",
          "message": "chore: leave some TODO's for rule loading (#6872)\n\nDocument a a few places where our rule loading is not lazy enough and\r\nprevents all the rules in a particular directory from being loaded.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-12T15:20:48-06:00",
          "tree_id": "0d7dea5cf82ccbdb31b196fd888a9907fbc08943",
          "url": "https://github.com/ocaml/dune/commit/c2833c6aa56108731487f3840ff72ed29cff374b"
        },
        "date": 1673559793123,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "42.45738344034",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "43c211e8dfaa9fb8ef0a1a0a70db4397016dc6f5",
          "message": "chore: move melange rules to melange dir (#6859)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-01-13T08:35:46-06:00",
          "tree_id": "c5a5a9f5133209497691d848b73182072002ccde",
          "url": "https://github.com/ocaml/dune/commit/43c211e8dfaa9fb8ef0a1a0a70db4397016dc6f5"
        },
        "date": 1673621740396,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.539878767286666",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "80a0b792c135cb4a09649680e5a51674590262c1",
          "message": "test(rules): demonstrate rule loading bug (#6873)\n\nIf there's an invalid virtual library implementation, it's impossible to\r\nload any of the rules in the directory.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-13T08:36:31-06:00",
          "tree_id": "e842c24a35519b263a3ae1f9fe06e03ead42dcfc",
          "url": "https://github.com/ocaml/dune/commit/80a0b792c135cb4a09649680e5a51674590262c1"
        },
        "date": 1673621816283,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.70232940215333",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "b5e2d3ef2bbf6bada4fbdf163a5fcf2cec07acf9",
          "message": "refactor(rules): remove unused argument (#6877)\n\n[do_not_fail] is never passed when finding the instrumentation backend\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-13T10:49:34-06:00",
          "tree_id": "8b7aec6cf027010836642efa6478a24a2f0422a7",
          "url": "https://github.com/ocaml/dune/commit/b5e2d3ef2bbf6bada4fbdf163a5fcf2cec07acf9"
        },
        "date": 1673629931301,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.26034425712667",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@emillon.org",
            "name": "Etienne Millon",
            "username": "emillon"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "30d385f6cc44c485ecbb4f3282eabc3479003b41",
          "message": "Merge 3.6.2 changelog (#6874)\n\nSigned-off-by: Etienne Millon <me@emillon.org>\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>",
          "timestamp": "2023-01-13T16:10:32Z",
          "tree_id": "0ac1804c1307416f58ae49be780e88211f0c55e4",
          "url": "https://github.com/ocaml/dune/commit/30d385f6cc44c485ecbb4f3282eabc3479003b41"
        },
        "date": 1673629933088,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.91390320243333",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@emillon.org",
            "name": "Etienne Millon",
            "username": "emillon"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "c24c5a7ad782c61d8fa550c2353ba167a9e5896b",
          "message": "Add code of conduct (#6875)\n\nThis code of conduct lives in <https://github.com/ocaml/code-of-conduct>\r\nand has been discussed [in this\r\nthread](https://discuss.ocaml.org/t/ocaml-community-code-of-conduct/10494).\r\nIt has been adopted in ocaml/ocaml in ocaml/ocaml#11761, and after a\r\ndiscussion between project maintainers it is now enabled in dune as\r\nwell.\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>",
          "timestamp": "2023-01-13T16:10:48Z",
          "tree_id": "d80d46dc7defd1766f7dfd1ebe690747436baf88",
          "url": "https://github.com/ocaml/dune/commit/c24c5a7ad782c61d8fa550c2353ba167a9e5896b"
        },
        "date": 1673629951942,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.283180139686664",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "9ca0f2c1a08b358c428abb6017b985f84c24e1c6",
          "message": "chore: add a comment about a race condition (#6876)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-13T10:30:11-06:00",
          "tree_id": "8442725c28ad7c3aced33a1888f5f42629a62afc",
          "url": "https://github.com/ocaml/dune/commit/9ca0f2c1a08b358c428abb6017b985f84c24e1c6"
        },
        "date": 1673630059002,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.963501130026664",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "bcb65745c86faeb31fbc292a8f05a466d8b890d1",
          "message": "refactor(rules): remove unnecessary field (#6878)\n\n[instrument_with] is already used inside [lib_config]\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-13T10:32:48-06:00",
          "tree_id": "a955583efb6083b45182164cffabfb3d67dd95f3",
          "url": "https://github.com/ocaml/dune/commit/bcb65745c86faeb31fbc292a8f05a466d8b890d1"
        },
        "date": 1673630060076,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.9350002681",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "javier.chavarri@gmail.com",
            "name": "Javier Chávarri",
            "username": "jchavarri"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "5030332dcd044d8fd6c7c0b90b81596d5cf8c6b1",
          "message": "test(melange): add test with copy_files (#6701)\n\ncopy_files and include_subdirs\r\n\r\nSigned-off-by: Javier Chavarri <javier.chavarri@gmail.com>",
          "timestamp": "2023-01-13T10:35:00-06:00",
          "tree_id": "cacb477d5dc33145a0f44e637269b8f1515f2d53",
          "url": "https://github.com/ocaml/dune/commit/5030332dcd044d8fd6c7c0b90b81596d5cf8c6b1"
        },
        "date": 1673630072942,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "36.43197574551334",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "fae5fb59a6776bf8804239083e5a552fd3cb3ebd",
          "message": "chore(rules): leave source gathering TODO (#6880)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-13T18:11:38-06:00",
          "tree_id": "9f2682529922746e5858a2f145dd2e5da514a551",
          "url": "https://github.com/ocaml/dune/commit/fae5fb59a6776bf8804239083e5a552fd3cb3ebd"
        },
        "date": 1673656772493,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "32.943001525393335",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "d256c7bbba090750988d3943418bdec7bd9c3250",
          "message": "refactor(rules): bootstrap info (#6881)\n\nonly pass the linking closure because that is the only thing that is\r\nbeing used.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-13T18:10:51-06:00",
          "tree_id": "b405f99fa27b1babda6202ac7d5873cd08d4ddd9",
          "url": "https://github.com/ocaml/dune/commit/d256c7bbba090750988d3943418bdec7bd9c3250"
        },
        "date": 1673656784308,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "36.24179583137334",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "3fd3e01fc9c2dfaf7e9abce7a76de76f9dfc722a",
          "message": "refactor: invert some if not .. else expressions (#6882)\n\nthey are just harder to read in all these cases\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-13T18:10:14-06:00",
          "tree_id": "c4e618bdd5c184eb870da76db97d14c58f77405d",
          "url": "https://github.com/ocaml/dune/commit/3fd3e01fc9c2dfaf7e9abce7a76de76f9dfc722a"
        },
        "date": 1673656978408,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "41.11123451716001",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "4a518c0fbbde5c6cbeeb8e20c024dfe0618d9644",
          "message": "refactor(rules): ues [Option.map] (#6879)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>\r\n\r\n<!-- ps-id: 2700fab2-6025-40e2-996c-ae1a9f77091c -->",
          "timestamp": "2023-01-13T18:11:13-06:00",
          "tree_id": "49fc617ee3f9ff225e42ec691507bdb26cc1e078",
          "url": "https://github.com/ocaml/dune/commit/4a518c0fbbde5c6cbeeb8e20c024dfe0618d9644"
        },
        "date": 1673657103743,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "43.537363901240006",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "98bf1d05f53f5ae6a64bd82ac25266f108e05ec7",
          "message": "refactor(rpc): put menu into rpc session (#6803)\n\nrather than managing it through a callback\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-16T10:27:34-06:00",
          "tree_id": "366a5f828eeddee3ae0c93c8d99a6092c9a0e4ad",
          "url": "https://github.com/ocaml/dune/commit/98bf1d05f53f5ae6a64bd82ac25266f108e05ec7"
        },
        "date": 1673887561402,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.15521081666666",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "3521dfa4144ed60fed047769031cba3432531a62",
          "message": "fix(scheduler): flush stats when idle (#6891)\n\n[dune --trace-file] currently relies on the output channel to flush the\r\nevents when it's internal buffer is full. This isn't a problem for a\r\nnormal build, because we only want to observe the trace file once dune\r\nterminates, but it is a problem for watch mode. In watch mode, we have\r\nto wait an arbitrary amount of time until the buffer gets filled up and\r\nis flushed.\r\n\r\nThis commit flushes the events output channel in watch mode whenever\r\nwe're idling and waiting for file system events to arrive.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-16T11:13:44-06:00",
          "tree_id": "e52bcfaac68ba5e09e966306965e5858e175fe34",
          "url": "https://github.com/ocaml/dune/commit/3521dfa4144ed60fed047769031cba3432531a62"
        },
        "date": 1673890527740,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.36363642134",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "0972be85f53c71e71a8328842c9555301f101d42",
          "message": "refactor: Build_system.Error.info (#6888)\n\nPreviously, [Build_system.Error.info] would return a tuple that was hard\r\nto read. We convert it into a record to make it easier.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-16T11:23:35-06:00",
          "tree_id": "a8a2d812aac31956ac7ac2052e0dd068e6fc665b",
          "url": "https://github.com/ocaml/dune/commit/0972be85f53c71e71a8328842c9555301f101d42"
        },
        "date": 1673891264556,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "44.922634288199994",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "073778234aeccfa125b6df14af0f88a2e431166a",
          "message": "fix(melange): include_subdirs (#6811)\n\ncopy the file path from the source\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-16T15:19:04-06:00",
          "tree_id": "2e477528b11339cf6fc62d5a0f82cdf3503eb06c",
          "url": "https://github.com/ocaml/dune/commit/073778234aeccfa125b6df14af0f88a2e431166a"
        },
        "date": 1673905088044,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.251742597926665",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "anmonteiro@gmail.com",
            "name": "Antonio Nuno Monteiro",
            "username": "anmonteiro"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "47150f22336913a3e356bc812bd4b887e483f4fc",
          "message": "fix(melange): switch `--bs-package-name` to library name (#6841)\n\nUse the public library name rather than the package name to determine the path under `node_modules/`.\r\n\r\nThis allows public melange libraries from the same package to co-exist in the same `melange.emit`.\r\n\r\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-16T19:10:40-06:00",
          "tree_id": "7276457f9ce78469124b565c8893c713382b4ce8",
          "url": "https://github.com/ocaml/dune/commit/47150f22336913a3e356bc812bd4b887e483f4fc"
        },
        "date": 1673918944406,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.481419861499994",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "73a3b3bac4ffeba5143a4e1a2e9bccb5c7eda815",
          "message": "chore: move jsoo rules to jsoo dir (#6861)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-01-16T19:13:10-06:00",
          "tree_id": "90807e7dc91ddb1d56842c92d8d10a63ed9b78db",
          "url": "https://github.com/ocaml/dune/commit/73a3b3bac4ffeba5143a4e1a2e9bccb5c7eda815"
        },
        "date": 1673919257937,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "36.89105236751333",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "23d3f261e890dadf0590b7c192424facddad8d56",
          "message": "fix(metrics): output correct process metrics (#6892)\n\nBefore this commit we would output an \"async\" start event when a process\r\nawould start nd then a \"complete\" event when it would be finished.\r\n\r\nThe \"async\" start event is unnecessary and this commit removes it. All\r\nthe information recorded in the \"async\" start event is therefore moved\r\nto the complete event.\r\n\r\nThe new output is now properly displayed by the various visualization\r\ntools (perfetto, chrome)\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-16T19:23:49-06:00",
          "tree_id": "6d55dc3ae1524842a0912a5a7d0cb9d539a3517d",
          "url": "https://github.com/ocaml/dune/commit/23d3f261e890dadf0590b7c192424facddad8d56"
        },
        "date": 1673919777032,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "36.404218036846665",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "7d11af7c84208239f9c705a542ef3fc0409469f7",
          "message": "chore: improve error messages with 2 locations (#6890)\n\nThe location of both of the libraries can be included by adding one of\r\nthe libraries as a \"related\" error. The end result is that both error\r\nmessages will be available to jump in rpc clients that consume\r\ndiagnostics\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-17T08:29:19-06:00",
          "tree_id": "076695c0a72c4c4f6dbd395e70dc64c8dc8b5b03",
          "url": "https://github.com/ocaml/dune/commit/7d11af7c84208239f9c705a542ef3fc0409469f7"
        },
        "date": 1673966897969,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.733694540959995",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "dfa29d3bb507247da3b9c15d8aaa741467d15077",
          "message": "test: wait for dune shutdown (#6898)\n\nModify stop_dune to wait until dune actually shuts down.\r\n\r\nThis is needed for tests such as stray-process.t which rely on their\r\nassertions to run after dune's finished.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-17T20:24:23-06:00",
          "tree_id": "3977c19c50aa139dc0a760fa4eb4bfaf8e63eeb3",
          "url": "https://github.com/ocaml/dune/commit/dfa29d3bb507247da3b9c15d8aaa741467d15077"
        },
        "date": 1674009788129,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.43978299247333",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "ecd5ce0d462b83370f7da35238fe1b15753d61f9",
          "message": "refactor: invert if statement in action_to_sh (#6896)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-17T20:23:29-06:00",
          "tree_id": "0c84bb31ac6d65a5f887500f21c3113910097bee",
          "url": "https://github.com/ocaml/dune/commit/ecd5ce0d462b83370f7da35238fe1b15753d61f9"
        },
        "date": 1674009980702,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "43.74642050798",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "70e0c6579de48ea10d69fbca04e7026e61c21f52",
          "message": "test: reproduce github 6866 (#6901)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-18T14:56:20-06:00",
          "tree_id": "c5a0d1c79c8a6c74b203027ef3f15903ea80061c",
          "url": "https://github.com/ocaml/dune/commit/70e0c6579de48ea10d69fbca04e7026e61c21f52"
        },
        "date": 1674076750036,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "42.38593950512",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "e9004020dd78762bbdf4bf37248da1c01b51c159",
          "message": "fix: (include_subdirs qualified) and pp (#6902)\n\nTo store the output of ocamldep, we'd choose the following path:\r\n\r\n$obj_dir/$module-basename.$ext.d\r\n\r\nThis scheme doesn't work for (include_subdirs qualified) because base\r\nfilenames are no longer unique.\r\n\r\nWe now choose the file path to be $obj_dir/%module-obj-name.$kind.d\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-19T10:50:49-06:00",
          "tree_id": "4fc078881133e0526b8e27013b62f9099248a88a",
          "url": "https://github.com/ocaml/dune/commit/e9004020dd78762bbdf4bf37248da1c01b51c159"
        },
        "date": 1674148578362,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "42.20826379258",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "415d9c482138c73814abdbbc6d6d53cfde2d35a1",
          "message": "test(rules): include_subdirs qualified (#6903)\n\nTest the case where a single logical module is defined both using a\r\ngroup of modules in a directory and a compilation unit\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-19T13:05:45-06:00",
          "tree_id": "69d6fc5a6549866637d3f9a0fe13414e234c3a63",
          "url": "https://github.com/ocaml/dune/commit/415d9c482138c73814abdbbc6d6d53cfde2d35a1"
        },
        "date": 1674156523934,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.98529872535334",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "hugo.heuzard@gmail.com",
            "name": "Hugo Heuzard",
            "username": "hhugo"
          },
          "committer": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "distinct": true,
          "id": "c357cee46f06e2c3c1f3dbbc396c96c12d23e2b3",
          "message": "fix(jsoo): don't ignore linkall\n\nSigned-off-by: Hugo Heuzard <hugo.heuzard@gmail.com>",
          "timestamp": "2023-01-19T18:02:29-06:00",
          "tree_id": "8a05b9fa951495f5a63c41729b5cf7ac9f3e2cd9",
          "url": "https://github.com/ocaml/dune/commit/c357cee46f06e2c3c1f3dbbc396c96c12d23e2b3"
        },
        "date": 1674174449379,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "37.408674639286666",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "0fe12038a2657ffc9d4ebd9cc28408d591f4f818",
          "message": "chore(nix): update flakes (#6906)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-19T20:37:35-06:00",
          "tree_id": "44e3fec7d0f473317fa309bbfe7a4d0edd2bd394",
          "url": "https://github.com/ocaml/dune/commit/0fe12038a2657ffc9d4ebd9cc28408d591f4f818"
        },
        "date": 1674184325649,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.63375980721333",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "0d44bbfdb2a68907a464aeb2dabe95388dac5712",
          "message": "fix(rules): include_subdirs qualified stdlib (#6904)\n\n(include_subdirs qualified) is forbidden for libraries with [stdlib]\r\n\r\nWe add a proper error message to reflect that\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-20T14:27:59-06:00",
          "tree_id": "a4649b4df87442097fcc9a7a5c2cb0de99d66ffb",
          "url": "https://github.com/ocaml/dune/commit/0d44bbfdb2a68907a464aeb2dabe95388dac5712"
        },
        "date": 1674247622844,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "36.29310759821333",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "1e49970121c8e6c899b71da02276938f9e32525d",
          "message": "fix(rules): include_subdirs qualified overlap (#6905)\n\n(include_subdirs qualified) cannot handle a directory and a module\r\ncorresponding to the same module path. For example, the module foo.ml\r\nand the module in foo/.\r\n\r\nWe now raise a good error message whenever we encounter this case.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-20T14:58:29-06:00",
          "tree_id": "ad40b5136ed1e6ec76d4a3959f3424c9c8323865",
          "url": "https://github.com/ocaml/dune/commit/1e49970121c8e6c899b71da02276938f9e32525d"
        },
        "date": 1674249448646,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.61184673049333",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "1513847d3db43604ca7a3d5c2004928b668e3d92",
          "message": "fix(rules): include_subdirs qualified and unwrapped (#6899)\n\nAllow (include_subdirs unqualified) for libraries that have (wrapped\r\nfalse)\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-20T21:29:05-06:00",
          "tree_id": "76580725842509510913dfabee9d81e53661381d",
          "url": "https://github.com/ocaml/dune/commit/1513847d3db43604ca7a3d5c2004928b668e3d92"
        },
        "date": 1674272888257,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "36.534778438046665",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "committer": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "fc0769e9319a0dfbf0a1ffca6d0cb4692e2315f4",
          "message": "refactor(coq): reintroduce Value type for coq_config\n\nAs was pointed out in my silly PR getting rid of it, it introduces an\nextra boxing that is unneeded.\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>\n\n<!-- ps-id: 2f6b0e94-1350-446a-810d-bfc6c34ae46e -->",
          "timestamp": "2023-01-21T04:34:29+01:00",
          "tree_id": "58b8d3b3042f12e85aa6b29e044126c480cd7d0b",
          "url": "https://github.com/ocaml/dune/commit/fc0769e9319a0dfbf0a1ffca6d0cb4692e2315f4"
        },
        "date": 1674273163977,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "32.92533258494",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "anmonteiro@gmail.com",
            "name": "Antonio Nuno Monteiro",
            "username": "anmonteiro"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "b2bfb9829b0c7d39a64d0467c0989b7885100831",
          "message": "doc: fix ppxfind link, OCaml typer typo (#6914)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-01-21T21:09:32-06:00",
          "tree_id": "225d8b7f6d39e1924f03f38b8c4b25829a101a87",
          "url": "https://github.com/ocaml/dune/commit/b2bfb9829b0c7d39a64d0467c0989b7885100831"
        },
        "date": 1674358095264,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.106447087733336",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "hugo.heuzard@gmail.com",
            "name": "hhugo",
            "username": "hhugo"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "23e09fd8abdc5e14401a7ae6759166ebc5890288",
          "message": "Fix jsoo linkall detection (#6916)\n\nSigned-off-by: Hugo Heuzard <hugo.heuzard@gmail.com>",
          "timestamp": "2023-01-22T20:50:11-06:00",
          "tree_id": "2d80d0e59f44a0a3137f03326f5da3e9662dd016",
          "url": "https://github.com/ocaml/dune/commit/23e09fd8abdc5e14401a7ae6759166ebc5890288"
        },
        "date": 1674443717333,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "37.103589204626665",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "ggreif@gmail.com",
            "name": "Gabor Greif",
            "username": "ggreif"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "e98aa363f26b97172b356055e5d76556204da583",
          "message": "Delete dune~ (#6923)\n\nLing path `test/blackbox-tests/test-cases/jsoo/no-check-prim.t/lib/dune~`\r\nThis was added along #5049 in error.\r\n\r\nSigned-off-by: Gabor Greif <gabor@dfinity.org>",
          "timestamp": "2023-01-23T20:56:08-06:00",
          "tree_id": "73c83fb5b5609d70fe8f6c1d32e2eabb6cd5a2f8",
          "url": "https://github.com/ocaml/dune/commit/e98aa363f26b97172b356055e5d76556204da583"
        },
        "date": 1674530089852,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.296092162633336",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@emillon.org",
            "name": "Etienne Millon",
            "username": "emillon"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "97a9f4620e1d2a802b55f6a22f9de3cb006b06c4",
          "message": "doc: use production lists (#6910)\n\n* doc: use production lists\r\n\r\nInstead of using code blocks, we use sphinx's built-in support for this.\r\nThis looks slightly better but the advantage is that we can use the\r\nfollowing syntax to refer to items:\r\n\r\n    :token:`dep`\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>",
          "timestamp": "2023-01-24T17:40:06+01:00",
          "tree_id": "8e534391496fa61550ab395dcf712c2cda6d7345",
          "url": "https://github.com/ocaml/dune/commit/97a9f4620e1d2a802b55f6a22f9de3cb006b06c4"
        },
        "date": 1674579531348,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.72972861526666",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "committer": {
            "email": "e+git@x80.org",
            "name": "Emilio Jesús Gallego Arias",
            "username": "ejgallego"
          },
          "distinct": true,
          "id": "ce578be06da8a78655200c56b345ed9e50be9dec",
          "message": "feature(coq): omit -q flag during dune coq top\n\n<!-- ps-id: 5a03ba95-9ec7-41fd-8f3f-db31c6742042 -->\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-01-24T20:06:05+01:00",
          "tree_id": "8aa397fd502060e8b974203fc4021ea1df2818b5",
          "url": "https://github.com/ocaml/dune/commit/ce578be06da8a78655200c56b345ed9e50be9dec"
        },
        "date": 1674588329840,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.0968027836",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "javier.chavarri@gmail.com",
            "name": "Javier Chávarri",
            "username": "jchavarri"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "e8ad4cd5db8b9b2c6071b91a7b94d81a82ef77f3",
          "message": "melange: add test for warning 102 (#6927)\n\nSigned-off-by: Javier Chávarri <javier.chavarri@gmail.com>",
          "timestamp": "2023-01-24T18:13:25-06:00",
          "tree_id": "cc209d5af17b729ebb0fb02cf8d8ccb8dabd642d",
          "url": "https://github.com/ocaml/dune/commit/e8ad4cd5db8b9b2c6071b91a7b94d81a82ef77f3"
        },
        "date": 1674606766323,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "36.912161040613334",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "071d3b13aa83648e8f3daed38c0b704e7d0144cc",
          "message": "test(diagnostics): multiple errors (#6935)\n\nTest multiple errors per compilation unit.\r\n\r\nAt the moment, they are ignored.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-25T17:58:37-06:00",
          "tree_id": "a637b2f73f521c1fd80c4e588d665e83e040688c",
          "url": "https://github.com/ocaml/dune/commit/071d3b13aa83648e8f3daed38c0b704e7d0144cc"
        },
        "date": 1674692551053,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.817644579273335",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "63fd699437e2adedbb3b27d94cdf8893d5f76f88",
          "message": "refactor: Dune_project.encode sanity (#6911)\n\ndune init relies on removing all dialects to make sure the serialized\r\ndune-project file doesn't have any dialects. That is weird because\r\nit explicitly relies on serializition/deserialization not being round trip.\r\n\r\nInstead of relying on this, we change the serializer to stop outputting\r\nthe set of dialects if it's equal to the default.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-26T20:46:05-06:00",
          "tree_id": "c721d5b4e4f04d2f6fef97d647437f9d0c2dbb96",
          "url": "https://github.com/ocaml/dune/commit/63fd699437e2adedbb3b27d94cdf8893d5f76f88"
        },
        "date": 1674788847546,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "39.93612355066667",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "9d5bccf72c5e1464473b935c1c280b948308c8d1",
          "message": "refactor(ocamlc_loc): fix lexer formatting (#6941)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-26T20:49:35-06:00",
          "tree_id": "0a32c653389226483f040ce3ecc3d785fd681288",
          "url": "https://github.com/ocaml/dune/commit/9d5bccf72c5e1464473b935c1c280b948308c8d1"
        },
        "date": 1674788862189,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "32.8996740538",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "8e35810f5f1d0cad2c96c8b50ed347834d8ca1be",
          "message": "feature(fiber): reimplement pools (#6814)\n\n* fix weird deadlocks\r\n* add better validation for invariants\r\n* make them a lot faster\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-27T01:14:46-06:00",
          "tree_id": "3aa12afe186392c602ea306fd771d3aef77638a4",
          "url": "https://github.com/ocaml/dune/commit/8e35810f5f1d0cad2c96c8b50ed347834d8ca1be"
        },
        "date": 1674804817764,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.89602029038",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "0192f78af51e461b2db1f9fb799779432bfe5d2c",
          "message": "test: diffing of inline tests is not concurrent (#6943)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-01-27T01:15:38-06:00",
          "tree_id": "99c361cada867d79c4e2752b4f52a6c19f57f0ee",
          "url": "https://github.com/ocaml/dune/commit/0192f78af51e461b2db1f9fb799779432bfe5d2c"
        },
        "date": 1674805079265,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "41.230985101593326",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "b9a8c9955ddd782a1d0a715bfd95ef2f8ae355b4",
          "message": "refactor(rules): unwrapped module layout (#6915)\n\nAdd type safety to the representation of (wrapped false) in the module\r\nlayout\r\n\r\nSuch a layout differs from (wrapped false) in that it never has top\r\nlevel module alias or toplevel interface module\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-27T10:19:18-06:00",
          "tree_id": "a93852a4ef4742e29746292346b214e5a470f17f",
          "url": "https://github.com/ocaml/dune/commit/b9a8c9955ddd782a1d0a715bfd95ef2f8ae355b4"
        },
        "date": 1674837461411,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.58693242094",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "ed14f7afa3500c0c907f0a3c8d27007664a9d658",
          "message": "test(rules): more alias module tests (#6924)\n\nTest the generated alias module for implementations of virtual libraries\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-27T10:20:56-06:00",
          "tree_id": "43f649ead5c93850817ea9f48ef5ae476a5817ce",
          "url": "https://github.com/ocaml/dune/commit/ed14f7afa3500c0c907f0a3c8d27007664a9d658"
        },
        "date": 1674837847441,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "43.5911258433",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "a41ab22afb1c964c40ef80c2affa0d4d403dca69",
          "message": "fix(rpc): bidirectional communication (#6801)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-27T11:22:16-06:00",
          "tree_id": "0520e55fe6b67240a31db91abafd66da7b533b50",
          "url": "https://github.com/ocaml/dune/commit/a41ab22afb1c964c40ef80c2affa0d4d403dca69"
        },
        "date": 1674841251576,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.03494551802667",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "3120c2e3adc9269d992ad21a68dfd952c4417995",
          "message": "remove fiber from public libraries (#6925)\n\nUndo a mistake a long time ago. Instead of releasing it from inside\r\ndune, this library will be released from a separate repository.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-27T11:32:41-06:00",
          "tree_id": "13e21754042167d72ef02f07e2bfd08266509d41",
          "url": "https://github.com/ocaml/dune/commit/3120c2e3adc9269d992ad21a68dfd952c4417995"
        },
        "date": 1674842105213,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "42.57085165099334",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "javier.chavarri@gmail.com",
            "name": "Javier Chávarri",
            "username": "jchavarri"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "bbcf9c97643ed0b9b3c4ad520ed225d01ebab10a",
          "message": "melange: test demonstrating issue with copy_files (#6946)\n\nSigned-off-by: Javier Chávarri <javier.chavarri@gmail.com>",
          "timestamp": "2023-01-27T12:23:32-06:00",
          "tree_id": "e86b1c259b0fd30156d97c26d117288063357687",
          "url": "https://github.com/ocaml/dune/commit/bbcf9c97643ed0b9b3c4ad520ed225d01ebab10a"
        },
        "date": 1674845273740,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "41.83956761713333",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "8484dd9f7b76207d760cf3e4218f5e17c488bec6",
          "message": "chore: move fiber tests to fiber/ (#6948)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-01-27T15:22:58-06:00",
          "tree_id": "3e3b353b9dd9e9b7b1fe944d8d16dbfdb26e7150",
          "url": "https://github.com/ocaml/dune/commit/8484dd9f7b76207d760cf3e4218f5e17c488bec6"
        },
        "date": 1674856028102,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "45.43961700963333",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "3607e08da44f13c4962114d671a496f9febec350",
          "message": "refactor: Ml_source.modules_of_obj_dir improve error (#6951)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-27T16:27:59-06:00",
          "tree_id": "a3b0a8540bdd88eb416fc07929f9056efa8c3d3a",
          "url": "https://github.com/ocaml/dune/commit/3607e08da44f13c4962114d671a496f9febec350"
        },
        "date": 1674859595356,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.29599071899333",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "corentin.leruth@gmail.com",
            "name": "Corentin Leruth",
            "username": "tatchi"
          },
          "committer": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "distinct": true,
          "id": "7698307cb825194b7abefabc74590d5e03276e40",
          "message": "fix: parsing of <= operator\n\nSigned-off-by: Corentin Leruth <corentin.leruth@gmail.com>",
          "timestamp": "2023-01-27T20:39:00-06:00",
          "tree_id": "114473d5c929244760560412490c1487bfd9c987",
          "url": "https://github.com/ocaml/dune/commit/7698307cb825194b7abefabc74590d5e03276e40"
        },
        "date": 1674874895051,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "43.56389777456667",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "fb16b363f57343f655b0ff7695f2b79ac44930fa",
          "message": "test: add example of error message that is not parsed (#6934)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-01-27T22:07:32-06:00",
          "tree_id": "6791f3505a8688e4c2812aebd21057de1beb5cdb",
          "url": "https://github.com/ocaml/dune/commit/fb16b363f57343f655b0ff7695f2b79ac44930fa"
        },
        "date": 1674879959278,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.83711165709333",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "hugo.heuzard@gmail.com",
            "name": "Hugo Heuzard",
            "username": "hhugo"
          },
          "committer": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "distinct": true,
          "id": "86bc1bf844591e41e84b98baca387c523628512b",
          "message": "feature(jsoo): recognize toplevel variant\n\nSigned-off-by: Hugo Heuzard <hugo.heuzard@gmail.com>",
          "timestamp": "2023-01-27T22:17:59-06:00",
          "tree_id": "0d27129d8ce2acff4218972523941bafff3d9ba5",
          "url": "https://github.com/ocaml/dune/commit/86bc1bf844591e41e84b98baca387c523628512b"
        },
        "date": 1674880578085,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.13476247242",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "3e27f8f2ebdc959ffac1a806e19062c214326e0b",
          "message": "chore: update pp and remove redundant tests (#6954)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-01-27T23:07:35-06:00",
          "tree_id": "9c1a8a5afd1c167cda3c02fd0848c2e731d3a94f",
          "url": "https://github.com/ocaml/dune/commit/3e27f8f2ebdc959ffac1a806e19062c214326e0b"
        },
        "date": 1674883565812,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.191898454193336",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "71855677+jonahbeckford@users.noreply.github.com",
            "name": "jonahbeckford",
            "username": "jonahbeckford"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "529d9c0cd43e2c9e0ed3c616511df404cc0c0109",
          "message": "Add 4.14.0 MSVC to CI (#6917)\n\n* Rename MSVC CI to MSVC 4.12.1 CI\r\n* Add MSVC 4.14.0 CI\r\n\r\nSigned-off-by: Jonah Beckford <71855677+jonahbeckford@users.noreply.github.com>",
          "timestamp": "2023-01-28T12:07:45+01:00",
          "tree_id": "1721b778f66849bc348c8b534daaf960c6c55589",
          "url": "https://github.com/ocaml/dune/commit/529d9c0cd43e2c9e0ed3c616511df404cc0c0109"
        },
        "date": 1674905174299,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "32.80608844677334",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "0e863c4d53d6d209d54a2fd4e50609b08726a53b",
          "message": "chore: move stdune tests to stdune/ (#6949)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-01-28T11:28:23-06:00",
          "tree_id": "f469b5b78b883707538ef55d66ba0a95e428b290",
          "url": "https://github.com/ocaml/dune/commit/0e863c4d53d6d209d54a2fd4e50609b08726a53b"
        },
        "date": 1674928030123,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.94107106030667",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "e99fbd1432ca82485ec025798ba2d4bdedc1bd7d",
          "message": "test(melange): fix copy-files-lib test (#6952)\n\n* Refer to to artifacts using %{project_root}\r\n* Add dependency for the melange alias on the target rather than the\r\n  dependency\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-28T11:27:11-06:00",
          "tree_id": "e80bee0ab3bc5d2d6f9504686e4fc23b6e09d318",
          "url": "https://github.com/ocaml/dune/commit/e99fbd1432ca82485ec025798ba2d4bdedc1bd7d"
        },
        "date": 1674928157922,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "40.54274793360667",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "5642908a96e53f3f4b507ad1b73d43f71a5187b0",
          "message": "refactor(rules): workspace improvements (#6957)\n\n* Improve the [to_dyn] functions\r\n* Rename [t] to [decode] as in the rest of the code base\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-28T17:10:00-06:00",
          "tree_id": "8d67554bb693a760eb6cc1775a45fc38e04a2b4d",
          "url": "https://github.com/ocaml/dune/commit/5642908a96e53f3f4b507ad1b73d43f71a5187b0"
        },
        "date": 1674948491000,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "32.642195425513336",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "f65bbb4cca33b2d89a1b82353d510e6ae5caedfc",
          "message": "test(rules): reproduce #6843 (#6956)\n\nCross compilation setup causes dune to crash\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-28T17:10:24-06:00",
          "tree_id": "5727b3239b05587ae559e2738f169341cd8822c2",
          "url": "https://github.com/ocaml/dune/commit/f65bbb4cca33b2d89a1b82353d510e6ae5caedfc"
        },
        "date": 1674948792009,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "42.0848505679",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "javier.chavarri@gmail.com",
            "name": "Javier Chávarri",
            "username": "jchavarri"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "714626f4d408e5c71c24ba91d0d520588702ec52",
          "message": "melange: clean up copy_files tests (#6955)\n\nSigned-off-by: Javier Chávarri <javier.chavarri@gmail.com>",
          "timestamp": "2023-01-29T11:13:39-06:00",
          "tree_id": "91a962ec8da8f8a5b29a1f6eef03dd4341dfbb67",
          "url": "https://github.com/ocaml/dune/commit/714626f4d408e5c71c24ba91d0d520588702ec52"
        },
        "date": 1675013532412,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.77530272876",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "eb6479bd19035fa935acb4cdd28c78e40ad63445",
          "message": "refactor(rules): modify generated source name (#6922)\n\nWe choose the source name of generated files to be basesd on the object\r\nname rather than the module name.\r\n\r\nWith (include_subdirs qualified), it's easy to generate a source name\r\nthat will collide with anothera generated module's name.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-29T15:45:38-06:00",
          "tree_id": "66777bba163b2eafc5d842b26974cc0e7a36ecb1",
          "url": "https://github.com/ocaml/dune/commit/eb6479bd19035fa935acb4cdd28c78e40ad63445"
        },
        "date": 1675030412288,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.27628809563999",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "a3280fb900fec7fa6ef570be17441223844ab4b8",
          "message": "chore(engine): Add [Rules.to_dyn] (#6964)\n\nuseful for printf debugging\r\n\r\nalso rename [data_to_dyn] to [dyn_of_data] to match our convention\r\nelsewhere.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-29T18:53:10-06:00",
          "tree_id": "4d475732d140d5125561a69ca0003e12029fd965",
          "url": "https://github.com/ocaml/dune/commit/a3280fb900fec7fa6ef570be17441223844ab4b8"
        },
        "date": 1675041418395,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "44.2109693038",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "d5fa2d35a4ebace945d34cb574d5f06c7223c8ea",
          "message": "chore(nix): update flakes (#6965)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-29T22:49:33-06:00",
          "tree_id": "045856cec0df3cbcd33427a7bfdd94ad83241f02",
          "url": "https://github.com/ocaml/dune/commit/d5fa2d35a4ebace945d34cb574d5f06c7223c8ea"
        },
        "date": 1675055290543,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.79529655743334",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "4eb93fddd3031bfa6fc5bc69b9ca4811e4aed650",
          "message": "fix(rules): canonical paths in aliases (#6963)\n\nThe canonical paths were all wrong for modules with (include_subdirs\r\nqualified). This PR addresses the problem.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-30T00:46:29-06:00",
          "tree_id": "0b3fdec684b050ac3aab97dba47893f9695c9811",
          "url": "https://github.com/ocaml/dune/commit/4eb93fddd3031bfa6fc5bc69b9ca4811e4aed650"
        },
        "date": 1675062336972,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.06499752475333",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "ab455e3365e55d6ddcf1664a1aa95d24ed338660",
          "message": "fix(rules): cross compilation bug (#6958)\n\nIt's possible for a context with targets to be a cross compilation\r\ncontext for other contexts. Previously, we'd assume that wasn't the\r\ncase.\r\n\r\nfixes #6843\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-30T09:57:19-06:00",
          "tree_id": "6a7a044a113528aeedf2f52e565b6319f88ca4f1",
          "url": "https://github.com/ocaml/dune/commit/ab455e3365e55d6ddcf1664a1aa95d24ed338660"
        },
        "date": 1675095406288,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "37.165515788",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "a7a4daa45d5b855276dba2dfab114923f7d4e37c",
          "message": "fix: add display options in --help (#6912)\n\nWe now enumerate all the values in --display in dune --help.\r\n\r\nFixes #526\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-30T10:18:15-06:00",
          "tree_id": "e063a30b2bada5aee007475ab3a82ec05230df5d",
          "url": "https://github.com/ocaml/dune/commit/a7a4daa45d5b855276dba2dfab114923f7d4e37c"
        },
        "date": 1675096642952,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.98595699554",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "297b41e1dcb1489e3285e229a56dc4350205b9de",
          "message": "refactor: reorganize threading logic (#6783)\n\nWe reorganize the threading logic for threaded dune_console\r\nbackends. We also add stages for things like handling user events\r\nwhich will be useful later for the NoTTY TUI implementation.\r\n\r\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-01-30T10:19:51-06:00",
          "tree_id": "586f4b7504971f4033a30d2e8e7771da71e8608a",
          "url": "https://github.com/ocaml/dune/commit/297b41e1dcb1489e3285e229a56dc4350205b9de"
        },
        "date": 1675096787471,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.467968252573336",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "3f60761cb5201755a511a4adc4b2b406066125ff",
          "message": "feature(scheduler): events for watch mode (#6895)\n\nIntroduce an instant event for every watch mode iteration. This event\r\nallows us to separate build commmands from different iterations of the\r\npolling loop.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-30T10:29:29-06:00",
          "tree_id": "98bf140bf8d337e85f917c8c49262a53953e07fa",
          "url": "https://github.com/ocaml/dune/commit/3f60761cb5201755a511a4adc4b2b406066125ff"
        },
        "date": 1675097386740,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.29021889984667",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "3a5012e63c6aa382fa015bd845dd1d3b4965697f",
          "message": "scheduler: move display from scheduler to global clflag (#6854)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-01-30T10:34:28-06:00",
          "tree_id": "2b3a5daae14193951a2c531d78f6f6d61051c9ed",
          "url": "https://github.com/ocaml/dune/commit/3a5012e63c6aa382fa015bd845dd1d3b4965697f"
        },
        "date": 1675097591725,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.07204805699333",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "7e12e09588299840d98ae6e92b7a62a83dd11074",
          "message": "test(melange): clean up copy emit test (#6962)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-30T11:35:36-06:00",
          "tree_id": "edec27b6a62e9d71051c4de4b446fdbe38ae67cd",
          "url": "https://github.com/ocaml/dune/commit/7e12e09588299840d98ae6e92b7a62a83dd11074"
        },
        "date": 1675101270300,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.96113858136667",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "antonin@tarides.com",
            "name": "Antonin Décimo",
            "username": "MisterDA"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "efe8fd7d1bf3e488784c95f7ea2417d6854ae0bb",
          "message": "feature(mdx): add colors to console logs (#6462)\n\nIf Dune should ouput ANSI colors to stderr, so can mdx.\r\n\r\nSigned-off-by: Antonin Décimo <antonin@tarides.com>",
          "timestamp": "2023-01-30T11:53:21-06:00",
          "tree_id": "32b5cf62f88f0a08a2e491b24e4a83618fc10bf2",
          "url": "https://github.com/ocaml/dune/commit/efe8fd7d1bf3e488784c95f7ea2417d6854ae0bb"
        },
        "date": 1675103483099,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "32.820345155506665",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@emillon.org",
            "name": "Etienne Millon",
            "username": "emillon"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "bbd15e00f2278a0715825fb76225cea1a10c31b3",
          "message": "doc: clarify purpose of dune files under \"project\" (#6852)\n\n* doc: clarify purpose of dune files under \"project\"\r\n\r\nThis sentence means that stanzas go in `dune` - not that `dune`\r\nfiles are mandatory.\r\n\r\nFixes #6845\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>\r\n\r\n* Explain that every directory can have a dune file\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>\r\n\r\n---------\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>",
          "timestamp": "2023-01-31T13:55:40+01:00",
          "tree_id": "7ae695e7e99defee12e895a6b3098bce28638415",
          "url": "https://github.com/ocaml/dune/commit/bbd15e00f2278a0715825fb76225cea1a10c31b3"
        },
        "date": 1675171121123,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "36.46305108446666",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "32379afe36f2e9d511dd21d5ac17712bba9a1e9d",
          "message": "fix(melange): emit rule loading (#6953)\n\nCorrectly load emit rules. Given an emit stanza in $dir with $output:\r\n\r\n* We generate the .js rules in $dir/$output\r\n* Anything under $dir/$output, we will load the rules and also redirect\r\n  upwards until we encounter $dir/output\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-31T20:39:52-06:00",
          "tree_id": "c4193b71ce1cb7b87fa74007b45108025eacd9fe",
          "url": "https://github.com/ocaml/dune/commit/32379afe36f2e9d511dd21d5ac17712bba9a1e9d"
        },
        "date": 1675220919869,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "40.73483926191333",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "5ff2863f8f4316b78f8c835750deca63c6e6d095",
          "message": "refactor(engine): remove status line from display (#6968)\n\nWhether the display mode contains the status line isn't something that\r\nis ever inspect in the engine. We can safely move it to the\r\n[Dune_config] and leave the [Display] in [Dune_engine] to be the subset\r\nthat concerns the process display.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-01-31T21:20:42-06:00",
          "tree_id": "1c51923ef741a93191056afa9b5b612bbba8fb2f",
          "url": "https://github.com/ocaml/dune/commit/5ff2863f8f4316b78f8c835750deca63c6e6d095"
        },
        "date": 1675222833633,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.361718546586665",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "aacc3915c9014f6854073c23b3167ee0b9b1bb96",
          "message": "refactor(merlin): use |> where possible (#6971)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-01T09:33:04-06:00",
          "tree_id": "a98394d87ba404dc7f2f2c7fac2572546122f9eb",
          "url": "https://github.com/ocaml/dune/commit/aacc3915c9014f6854073c23b3167ee0b9b1bb96"
        },
        "date": 1675266711370,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.52550380869334",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "45d346e9c4c595dc2117ac17efb05d0c66138660",
          "message": "refactor(rules): simplify let bindings (#6972)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-01T09:36:02-06:00",
          "tree_id": "8172e726ee4611db0ed3ae56983973dcf1004f77",
          "url": "https://github.com/ocaml/dune/commit/45d346e9c4c595dc2117ac17efb05d0c66138660"
        },
        "date": 1675267038166,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "38.218640078906674",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "stephen@sherra.tt",
            "name": "Stephen Sherratt",
            "username": "gridbugs"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "d90ec87ed6221878a0d6055575fe89f371aee046",
          "message": "Defer computation of local_bins (#6764)\n\nThis fixes a memo dependency cycle between evaluating globs in install\r\nstanzas and populating the artifacts database. Populating the artifacts\r\ndatabase involves enumerating all files installed in the \"bin\" section\r\nwhich involves expanding globs as these files can be specified as globs\r\nrather than literal files. Expanding globs in the install stanza\r\nrequires loading the rules for the directory containing the glob, and\r\ndoing so depends on the artifacts database.\r\n\r\nSigned-off-by: Stephen Sherratt <stephen@sherra.tt>",
          "timestamp": "2023-02-01T09:54:07-06:00",
          "tree_id": "9836fdc2886decd6d736a326225694d575c26d11",
          "url": "https://github.com/ocaml/dune/commit/d90ec87ed6221878a0d6055575fe89f371aee046"
        },
        "date": 1675268249033,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "42.69017942142667",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "570ae9b7200719996e4209bd66a97c4bc58a6a2b",
          "message": "refactor(rules): add module paths to module sources (#6974)\n\nThe module paths are based on the source path of the module itself.\r\nTherefore, we should compute them as early as possible.\r\n\r\nThis will allow for doing various error checks that depend on the module\r\npath without constructing complete [Modules.t] values\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-01T19:27:20-06:00",
          "tree_id": "da2ab4bf917777487c47055fa21e94e71bfa2685",
          "url": "https://github.com/ocaml/dune/commit/570ae9b7200719996e4209bd66a97c4bc58a6a2b"
        },
        "date": 1675302642309,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "36.51417992059333",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "cd12b4fbaac5d14471bcf4df32c76d935837589a",
          "message": "test(rules): sources for (include_subdirs qualified) (#6976)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-01T19:47:15-06:00",
          "tree_id": "bdf00ce706d48a6f322364dfdeef8b13e3c287f2",
          "url": "https://github.com/ocaml/dune/commit/cd12b4fbaac5d14471bcf4df32c76d935837589a"
        },
        "date": 1675303779114,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "42.26555374730667",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@emillon.org",
            "name": "Etienne Millon",
            "username": "emillon"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "fba9ac9fd05bd9e04982749f304390668eb2d8c4",
          "message": "Doc: use a glossary (#6909)\n\n* Use a RST glossary\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>",
          "timestamp": "2023-02-02T18:08:34+01:00",
          "tree_id": "afeaf6c165868a727b6e166e361e1efb8cfd54b5",
          "url": "https://github.com/ocaml/dune/commit/fba9ac9fd05bd9e04982749f304390668eb2d8c4"
        },
        "date": 1675358908582,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.10262835242666",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "2098bb78e4936309088a42fcf4d8afe7ffdcacba",
          "message": "test(rules): include_subdirs and install (#6979)\n\nTests should exercise libraries without sources\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-02T11:59:28-06:00",
          "tree_id": "757d936ff0dabd7b9472bb4dfd38f87761e7c615",
          "url": "https://github.com/ocaml/dune/commit/2098bb78e4936309088a42fcf4d8afe7ffdcacba"
        },
        "date": 1675362073895,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "39.59517013021334",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "0e6104d89d79013d49fa4c2e906b30bb795204c9",
          "message": "fix(rule): include_subdirs and foreign sources (#6981)\n\nPreviously, we were not allowing `(include_subdirs qualified)` for\r\nforeign sources. Now that (include_subdirs qualified) is supported, this\r\ncheck doesn't make sense.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-02T13:04:16-06:00",
          "tree_id": "337fd1e19a12ba797e315a51c857273a29c36836",
          "url": "https://github.com/ocaml/dune/commit/0e6104d89d79013d49fa4c2e906b30bb795204c9"
        },
        "date": 1675365944501,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "43.1105617358",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "b3595bd2e9e23a78c79eadcf191e1deadc0b82e8",
          "message": "refactor(rules): remove [as_in_build_dir_exn] (#6983)\n\nit's used in a useless way\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-02T19:20:06-06:00",
          "tree_id": "4dcb54fb3b340ace2cdc5d35eca616cfd4bc2411",
          "url": "https://github.com/ocaml/dune/commit/b3595bd2e9e23a78c79eadcf191e1deadc0b82e8"
        },
        "date": 1675388310969,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.35471249695333",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "80c57c97f4c0fc0f87405a916e7f1983575559ef",
          "message": "refactor(rules): simplify with List.remove_last_exn (#6973)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-02T19:22:46-06:00",
          "tree_id": "95f41dc0cc565487225cf1f0e11d3970b6d7bb23",
          "url": "https://github.com/ocaml/dune/commit/80c57c97f4c0fc0f87405a916e7f1983575559ef"
        },
        "date": 1675388489326,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.68161466124667",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "577f286ed45171123fae2e68e018560af72fcbb6",
          "message": "refactor(rules): remove relocate_alias_module (#6977)\n\nRather than creating the alias module in the source directory, and then\r\nmoving it to the object directory, we just create it directly in the\r\nobject directory.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-02T19:20:31-06:00",
          "tree_id": "63636579d50197ae98387fe90f8a3b0c74081557",
          "url": "https://github.com/ocaml/dune/commit/577f286ed45171123fae2e68e018560af72fcbb6"
        },
        "date": 1675388514974,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "41.89780935126",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "9f2b230f00a04e7a630961c9374fd20469d65ad0",
          "message": "refactor: use [Filename.t] instead of [string] (#6984)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-03T09:24:14-06:00",
          "tree_id": "5c3f67a89ca24a880ec85448aa06483b83119ac5",
          "url": "https://github.com/ocaml/dune/commit/9f2b230f00a04e7a630961c9374fd20469d65ad0"
        },
        "date": 1675438976706,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.76949028542666",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "eb5379322c2326f9cd4c364a6a57c9dbcc2ed4c8",
          "message": "refactor: use and improve [Dune_project.is_extension_set] (#6985)\n\n* Implement it with Univ_map.mem\r\n* Use is consistently everywhere\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-03T09:24:38-06:00",
          "tree_id": "e0e02bb8a8bede345b2c28407f17f1e8b3924e28",
          "url": "https://github.com/ocaml/dune/commit/eb5379322c2326f9cd4c364a6a57c9dbcc2ed4c8"
        },
        "date": 1675438982446,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.46949026118",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "c3352d8ad2f208615ae85d4b1d06fbf00b783dab",
          "message": "test: move toplevel tests to one directory (#6990)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-03T19:02:24-06:00",
          "tree_id": "1b38c258a53e305774ee97a6065c1d035a6f6370",
          "url": "https://github.com/ocaml/dune/commit/c3352d8ad2f208615ae85d4b1d06fbf00b783dab"
        },
        "date": 1675474583629,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "43.626043975660004",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "3022e87b2844e7f64c7bc6ab3d8ff882f592ef8f",
          "message": "refactor(console): split interface (#6989)\n\nSplit the interface to multiple files and allow threaded backends to be\r\ndefined outside the [dune_console]\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-03T22:02:26-06:00",
          "tree_id": "c1a1ef4d5e6e933a3cda4c7925189fb30aee008b",
          "url": "https://github.com/ocaml/dune/commit/3022e87b2844e7f64c7bc6ab3d8ff882f592ef8f"
        },
        "date": 1675484537062,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "38.77953160348667",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "f7cf8d45c850fc86584ce8fc38b4b249cadd485a",
          "message": "fix: pre-emptively clear screen (#6987)\n\nWhen running with fancy terminals in watch mode, we pre-emptively clear\r\nthe screen. This is to prevent subsequent clears in watch mode making\r\nthe user's screen jump.\r\n\r\nFixes #6884\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-03T23:17:48-06:00",
          "tree_id": "aabad920f45702980d3885c314fc89e5f02bb0bc",
          "url": "https://github.com/ocaml/dune/commit/f7cf8d45c850fc86584ce8fc38b4b249cadd485a"
        },
        "date": 1675488995551,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.62773831094667",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "n.oje.bar@gmail.com",
            "name": "Nicolás Ojeda Bär",
            "username": "nojb"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "93815730c4d9b42ae0bf4bac051de18dec9c2a86",
          "message": "Unix.link: catch EMLINK also on Windows (#6993)",
          "timestamp": "2023-02-04T15:56:12+01:00",
          "tree_id": "30cf010c488e85fc210ee9c049ad5502f3b17aa3",
          "url": "https://github.com/ocaml/dune/commit/93815730c4d9b42ae0bf4bac051de18dec9c2a86"
        },
        "date": 1675523703902,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.083287552486674",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "165f735f775387dd80294327555f23a3e77ecd57",
          "message": "test: Demonstrate leaking of display for cram test (#7003)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-02-05T13:54:01-06:00",
          "tree_id": "d1f649de7560a6aa4d249e070bd0af875992c0ca",
          "url": "https://github.com/ocaml/dune/commit/165f735f775387dd80294327555f23a3e77ecd57"
        },
        "date": 1675628201447,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "43.842280371899996",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "n.oje.bar@gmail.com",
            "name": "Nicolás Ojeda Bär",
            "username": "nojb"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "7e26d337a96b8abb4ead126d847b8e6427f6613d",
          "message": "csexp_rpc.ml: small simplification (#7006)\n\n* No need to set fd to non-blocking\r\n* Unix.select should not raise EAGAIN\r\n\r\nSigned-off-by: Nicolás Ojeda Bär <n.oje.bar@gmail.com>",
          "timestamp": "2023-02-05T23:41:15+01:00",
          "tree_id": "d2e9decf2b8e5a3b5538cbd26b0fd84923fb437e",
          "url": "https://github.com/ocaml/dune/commit/7e26d337a96b8abb4ead126d847b8e6427f6613d"
        },
        "date": 1675637985598,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.42048447388",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "63cb1900a451f585b20b01e7e39e36947d712c4d",
          "message": "fix(ocamlc_loc): extended excerpts (#7008)\n\n[ocamlc_loc] would fail to parse excerpts that would contains dots in\r\nthem\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-05T17:21:08-06:00",
          "tree_id": "cd4a2788d304eab247ab7e179c48aa69271078d6",
          "url": "https://github.com/ocaml/dune/commit/63cb1900a451f585b20b01e7e39e36947d712c4d"
        },
        "date": 1675640411818,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "36.65358238688666",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "55a26e2c3e89b1d148906f910f788e4f32e8c40f",
          "message": "chore: cleanup uses for display (#6997)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-02-05T17:34:13-06:00",
          "tree_id": "ad551365e6bd503b12a369be73236f417b143474",
          "url": "https://github.com/ocaml/dune/commit/55a26e2c3e89b1d148906f910f788e4f32e8c40f"
        },
        "date": 1675641149339,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "32.032020218746666",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "a808932529dc260a92e876a3a187f61300c5e171",
          "message": "doc: fix cram test in doc (#6995)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>\r\n\r\n<!-- ps-id: 58b82cf3-71f5-4864-9360-d1db10644e2c -->",
          "timestamp": "2023-02-06T10:16:39+01:00",
          "tree_id": "8ca3b01cb5edf3d8b3c40a974ff7379698a538db",
          "url": "https://github.com/ocaml/dune/commit/a808932529dc260a92e876a3a187f61300c5e171"
        },
        "date": 1675676723119,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.44550600602667",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "moyodiallo@gmail.com",
            "name": "Alpha Issiaga DIALLO",
            "username": "moyodiallo"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "14daaf6f0f9b12769c7b07cae7309d4cce5ebc54",
          "message": "dune describe external-lib-deps: printing out more information (#6839)\n\n* Print out more information\r\n\r\nThe command print out more information, the package in which an external\r\nlibrary belongs to, the libraries, executables and tests at this point\r\nwith their respective external_lib_deps.\r\n\r\nIt was possible with the command `dune external-lib-deps` which was\r\nremoved, to use `@install` `@runtest` aliasis. And we ended up with the\r\nnew command to not be able to use those, this is why knowing the package\r\nof an external library could help.\r\n\r\nThe goal is to have much information for `opam-dune-lint`.\r\n\r\nSigned-off-by: Alpha DIALLO <moyodiallo@gmail.com>\r\n\r\n* Clean the code and fix the tests output\r\n\r\nSigned-off-by: Alpha DIALLO <moyodiallo@gmail.com>\r\n\r\n---------\r\n\r\nSigned-off-by: Alpha DIALLO <moyodiallo@gmail.com>\r\nCo-authored-by: Etienne Millon <me@emillon.org>",
          "timestamp": "2023-02-06T11:31:52+01:00",
          "tree_id": "755b68b4660f939d9a90ef37c5d541dcff479608",
          "url": "https://github.com/ocaml/dune/commit/14daaf6f0f9b12769c7b07cae7309d4cce5ebc54"
        },
        "date": 1675681153933,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.343671219120004",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@emillon.org",
            "name": "Etienne Millon",
            "username": "emillon"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "bc010606803128f40382e73da53238003229e9b4",
          "message": "Upgrade ocamlformat to 0.24.1 (#7011)\n\nSigned-off-by: Etienne Millon <me@emillon.org>",
          "timestamp": "2023-02-06T16:07:19+01:00",
          "tree_id": "6371125f980acfa8a5dc39b7d70db34bd19e1c7b",
          "url": "https://github.com/ocaml/dune/commit/bc010606803128f40382e73da53238003229e9b4"
        },
        "date": 1675697155437,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.10655592483334",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "3076e6770c11ce8917f6736abaeff237cee2a136",
          "message": "feature(rules): custom alias for cinaps (#6991)\n\nAllow setting the alias used to run cinaps actions.\r\n\r\nThis is done to override the behavior of attaching the cinaps action to\r\nboth the `cinaps` and `runtest` aliases.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-06T10:57:59-06:00",
          "tree_id": "fe6a017da3e2c242419092d6b8bcceac8bc08eed",
          "url": "https://github.com/ocaml/dune/commit/3076e6770c11ce8917f6736abaeff237cee2a136"
        },
        "date": 1675705823301,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.140991822240004",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "df834cb516d4045f8fb9cf4fa83912a258df4b9e",
          "message": "test: check cram disable doesn't fail silently for cram stanza (#7007)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-02-06T15:10:25-06:00",
          "tree_id": "659080ad5a03a377008752df6840c6c0b2641b83",
          "url": "https://github.com/ocaml/dune/commit/df834cb516d4045f8fb9cf4fa83912a258df4b9e"
        },
        "date": 1675719126317,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "39.748733140073334",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "46a4e2d3480c51df2347c7b7ed1f2859e9fc5f54",
          "message": "test: demonstrate the \"misc\" section (#7014)\n\nThe \"misc\" section isn't currently supported by dune. Probably because\r\nit breaks the sandboxing guarantees of switches.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-06T23:53:27-06:00",
          "tree_id": "b731234b57cf62a77d2784e75fa4eb234c4d6802",
          "url": "https://github.com/ocaml/dune/commit/46a4e2d3480c51df2347c7b7ed1f2859e9fc5f54"
        },
        "date": 1675750544585,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "41.07965119526667",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "javier.chavarri@gmail.com",
            "name": "Javier Chávarri",
            "username": "jchavarri"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "c8f392fe9f9a2374a15979220e2f15c0ba32cfbf",
          "message": "docs: add rule production docs (#7019)\n\nSigned-off-by: Javier Chávarri <javier.chavarri@gmail.com>",
          "timestamp": "2023-02-07T10:53:16-06:00",
          "tree_id": "c81dd67d2c9f9e095bea647a43f94e90bb6455a1",
          "url": "https://github.com/ocaml/dune/commit/c8f392fe9f9a2374a15979220e2f15c0ba32cfbf"
        },
        "date": 1675789925201,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.13129607949333",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "06dde72a18766eed8d3b6fc61eb6d990b14089e0",
          "message": "refactor(rules): remove pointless qualification (#7021)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-07T18:38:17-06:00",
          "tree_id": "7b51da44cb4accaa42d80641a6b194a40c2c4d70",
          "url": "https://github.com/ocaml/dune/commit/06dde72a18766eed8d3b6fc61eb6d990b14089e0"
        },
        "date": 1675818172867,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "43.62815600000666",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "jon@recoil.org",
            "name": "Jon Ludlam",
            "username": "jonludlam"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "1d5ce4a33b4f6b369c4fd2435aceb18cabd2c03b",
          "message": "odoc: Put support files in their own directory (#6913)\n\n* odoc: Put support files in their own directory\r\n\r\nodoc 2.2 added math support, which comes with extra javascript, css and\r\nfont files. Rather than add these as explicit targets, this commit\r\nchanges the invocation of `odoc support-files` to output into the dir\r\n`_build/default/_doc/_html/_odoc_support` as a directory target.\r\n\r\nSigned-off-by: Jon Ludlam <jon@recoil.org>\r\n\r\n* Set directory targets for the _odoc_support dir\r\n\r\nSigned-off-by: Jon Ludlam <jon@recoil.org>\r\n\r\n* More directory rules fixes for odoc\r\n\r\nSigned-off-by: Jon Ludlam <jon@recoil.org>\r\n\r\n* Formatting\r\n\r\nSigned-off-by: Jon Ludlam <jon@recoil.org>\r\n\r\n* Factor out constant\r\n\r\nSigned-off-by: Jon Ludlam <jon@recoil.org>\r\n\r\n* Update CHANGES\r\n\r\nSigned-off-by: Jon Ludlam <jon@recoil.org>\r\n\r\n---------\r\n\r\nSigned-off-by: Jon Ludlam <jon@recoil.org>",
          "timestamp": "2023-02-08T15:24:09+01:00",
          "tree_id": "b3569d7250ee44e306b02300fc3f49c3b3895c4f",
          "url": "https://github.com/ocaml/dune/commit/1d5ce4a33b4f6b369c4fd2435aceb18cabd2c03b"
        },
        "date": 1675867496280,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "38.60190044360667",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "0c1bbed64e1bcb522f91b1d378d1be44453d367e",
          "message": "test: virtual_modules and modules (#7024)\n\ndemonstrate what happens when we set a module to be virtual but not\r\ninclude it in (modules ..)\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-08T09:35:49-06:00",
          "tree_id": "b9f4bdb273aa2b7647254c1b9d9c1c45ed8af346",
          "url": "https://github.com/ocaml/dune/commit/0c1bbed64e1bcb522f91b1d378d1be44453d367e"
        },
        "date": 1675871681699,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.03638294725334",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "808a0e05308b1137902ba0f82dd6c1ec544497dd",
          "message": "test: modules_without_implementation and modules (#7022)\n\ndemonstrate the behavior when a module is excluded by (modules ..) but\r\nis written inside (modules_without_implementation ..)\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>\r\n\r\n<!-- ps-id: 8075951a-3306-48db-ba8e-423bac14a4c9 -->",
          "timestamp": "2023-02-08T09:37:09-06:00",
          "tree_id": "ea5121e537fb943f6c070033c63b416a11981113",
          "url": "https://github.com/ocaml/dune/commit/808a0e05308b1137902ba0f82dd6c1ec544497dd"
        },
        "date": 1675871774917,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.851500095093336",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "425895afb1e9246fcd8c7e6aee1fbc077c0c4b3a",
          "message": "test: modules and private_modules (#7023)\n\ndemonstrate the behavior when a module is listed by (private_modules ..)\r\nbut is exlucded by (modules ..)\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-08T09:37:27-06:00",
          "tree_id": "cafdcadd7a2afc57336a688050be92ba226d33a4",
          "url": "https://github.com/ocaml/dune/commit/425895afb1e9246fcd8c7e6aee1fbc077c0c4b3a"
        },
        "date": 1675871787338,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.053125297079994",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "a4d81e92a322d7357bb2ca4db1bc9e0e28e21f4b",
          "message": "test(melange): reproduce #7020 (#7025)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-08T09:45:28-06:00",
          "tree_id": "c7898e33ac90883267b0f6979c6f48d783dbcb6b",
          "url": "https://github.com/ocaml/dune/commit/a4d81e92a322d7357bb2ca4db1bc9e0e28e21f4b"
        },
        "date": 1675872496177,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "42.39977095604667",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "89115fef7cb2c89b66b233ad2569c54b637c142c",
          "message": "fix(rules): installing sources (#7005)\n\nPreserve module sources relative to the directory where the library is\r\ndefined when installing sources.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-08T15:14:11-06:00",
          "tree_id": "8dc36b63535e6e782114e24b5eb437f9e61c50ef",
          "url": "https://github.com/ocaml/dune/commit/89115fef7cb2c89b66b233ad2569c54b637c142c"
        },
        "date": 1675891970303,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.13300844956667",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "7c467c41bc0926f940500b1cb946adf5018b8fe7",
          "message": "test(rules): test all sources (#7030)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-08T16:00:53-06:00",
          "tree_id": "431ce1a4c4bd5bf6f97bfa11ad172a8040c328ef",
          "url": "https://github.com/ocaml/dune/commit/7c467c41bc0926f940500b1cb946adf5018b8fe7"
        },
        "date": 1675895240519,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "41.947884893313336",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "787c85d31460aadea12b6e1ef353256a2abfb867",
          "message": "fix(rules): custom install paths for modules (#7031)\n\nAllow setting custom installation paths for generated modules. This\r\nallows us use the group interface name when installing alias modules.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-08T16:50:37-06:00",
          "tree_id": "8bafbc9b74784dd7def004106a26710aba01aa19",
          "url": "https://github.com/ocaml/dune/commit/787c85d31460aadea12b6e1ef353256a2abfb867"
        },
        "date": 1675897758613,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.712778212686665",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "873c6ba90327b347524c39b2b09942c09dfd62ca",
          "message": "fix: include all diagnostics (#6940)\n\nSome compilation commands emit more than one diagnostics. For example,\r\nocamlc can emit more than one deprecation or unused error warning.\r\n\r\nThe previous behavior would be to just take the first error and drop the\r\nothers. This PR fixes the behavior to include all errors extracted out\r\nof a command.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-08T17:50:53-06:00",
          "tree_id": "49dde810846ac5c4e25f7b6d9526f832d76643fa",
          "url": "https://github.com/ocaml/dune/commit/873c6ba90327b347524c39b2b09942c09dfd62ca"
        },
        "date": 1675901360202,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "32.55434217541333",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@emillon.org",
            "name": "Etienne Millon",
            "username": "emillon"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "584d94cd7a8df7154b234f9eceedcdab59484939",
          "message": "ctypes 0.3: run commands in stanza directory (#6883)\n\nThis creates version 0.3 of the ctypes field.\r\n\r\nWhen used, commands are run in the directory where the corresponding\r\nstanza is defined. This makes it possible to use relative directories.\r\n\r\nFixes #5325\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>",
          "timestamp": "2023-02-09T11:25:22+01:00",
          "tree_id": "6311915bf0ab12a45e0a046d8382137a1311d21d",
          "url": "https://github.com/ocaml/dune/commit/584d94cd7a8df7154b234f9eceedcdab59484939"
        },
        "date": 1675939435484,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.3659258704",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "emile.trotignon@laposte.net",
            "name": "Emile Trotignon",
            "username": "EmileTrotignon"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "50eda639151a9a2eec402d8da916adcf3b51e136",
          "message": "Formatting stanza works with no arguments again (#7035)\n\nSigned-off-by: Emile Trotignon <emile@tarides.com>",
          "timestamp": "2023-02-09T15:50:15+01:00",
          "tree_id": "c1e4b40791d8bbc21e92e65b03a8b87b818c4dff",
          "url": "https://github.com/ocaml/dune/commit/50eda639151a9a2eec402d8da916adcf3b51e136"
        },
        "date": 1675955563552,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "40.19636600314001",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "efcafc0c01c8e2a277186d97e6bd28006d8d9cb7",
          "message": "fix: module stanza mapping (#7029)\n\nDo not use [Modules.t] to detect if a module is present in more than one\r\nstanza.\r\n\r\nThis is a source based check so we should just use the sources directly\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-09T10:19:39-06:00",
          "tree_id": "ad8af8bcde755f37cee58467f6321f5d9173c592",
          "url": "https://github.com/ocaml/dune/commit/efcafc0c01c8e2a277186d97e6bd28006d8d9cb7"
        },
        "date": 1675960914116,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "41.77806863774",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "111161d3c36c495cb5c5a68510a9533a316a0ad8",
          "message": "fix(melange): incompatible libraries (#7033)\n\nLibraries that are incompatible with the melange rules are:\r\n\r\n* Non dune libraries\r\n* Libraries defined with dune but installed with META templates\r\n\r\nWe emit a proper erorr message when we encounter such libraries\r\n\r\nIdeally, we should discover such incompatibilities where they first\r\nintroduced, but this is better than the current hideous error.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-09T10:28:32-06:00",
          "tree_id": "27d673e2ef47afef1bf6c727c2d7d16f43d29923",
          "url": "https://github.com/ocaml/dune/commit/111161d3c36c495cb5c5a68510a9533a316a0ad8"
        },
        "date": 1675961327625,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.03419351379333",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "richardlford@users.noreply.github.com",
            "name": "Richard L Ford",
            "username": "richardlford"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "7bdc8d6ecb97630b96b406d0e71b12bb833cbe41",
          "message": "Add dune-project stanza, map_workspace_root, to control workspace mapping. (#6988)\n\nAdd a \"map_workspace_root\" stanza to the dune-project file.\r\nIf true, references to the workspace root directory in output\r\nfiles are mapped to \"/workspace_root\". If false, such references\r\nare not modified.\r\nIf missing, it defaults to true.\r\n\r\nNote that in the added tests, for some configurations quotes\r\nare needed around the \"EOF\" delimeter to get expansion.\r\n\r\nNote also that when enabled, the debug search directories in the\r\ndebug information produced by ocamlc are also mapped, with\r\nthe result that ocamldebug cannot find the files.\r\n\r\nFixes #6929, provided user disables mapping.\r\n\r\nSigned-off-by: Richard L Ford <richardlford@gmail.com>\r\nCo-authored-by: Christine Rose <christinerose@users.noreply.github.com>\r\nCo-authored-by: Ali Caglayan <alizter@gmail.com>\r\nCo-authored-by: Etienne Millon <me@emillon.org>",
          "timestamp": "2023-02-09T20:20:07+01:00",
          "tree_id": "128f2c89d3417434b439ed4098d005a62382ebf9",
          "url": "https://github.com/ocaml/dune/commit/7bdc8d6ecb97630b96b406d0e71b12bb833cbe41"
        },
        "date": 1675971620632,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.01397091365333",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "5b386539f308f397238fda7bc9851e71e8b8caf5",
          "message": "test: reproduce #7018 (#7037)\n\nDemonstrate various types of module cycles that aren't cycles if we\r\nseparate the module interface and module implementation graphs\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-09T18:58:23-06:00",
          "tree_id": "8be61892ba1894b813b2bba5f8df3af26ee102b1",
          "url": "https://github.com/ocaml/dune/commit/5b386539f308f397238fda7bc9851e71e8b8caf5"
        },
        "date": 1675992033312,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.37642743406667",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "0b85f98655adf71731309232fd968c9fa14a4353",
          "message": "fix(rules): maintain modules sources when installing (#7041)\n\nWhen installing module sources, we would do the following:\r\n\r\n* Flatten them all into a single directory\r\n* Forget the original filenames (in particular, their case)\r\n\r\nThis PR saves this information when installing modules.\r\n\r\nThe information will be useful in two ways:\r\n\r\n* Recreating the directory structure when emitting .js for melange\r\n* Browsing installed sources will be less awkward\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-10T22:52:24-06:00",
          "tree_id": "158f4d39db3d6d01059b84181918d6730de69692",
          "url": "https://github.com/ocaml/dune/commit/0b85f98655adf71731309232fd968c9fa14a4353"
        },
        "date": 1676092623555,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.14857015765333",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "6ccd648267d672fc7bfde0042a77c6720b96567a",
          "message": "refactor(console): call start/finish (#6999)\n\nWhen setting the console backend to a value, call:\r\n\r\n* [finish] on the currently set backend\r\n* [start] on the incoming backend\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-11T11:00:24-06:00",
          "tree_id": "a7c590ef27cd127db2e7d58a73272b8229f1e8b0",
          "url": "https://github.com/ocaml/dune/commit/6ccd648267d672fc7bfde0042a77c6720b96567a"
        },
        "date": 1676135934620,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.782181382193336",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "7cc9523524450417f306b5d3f4a1acbd39d88472",
          "message": "test(melange): include_subdirs and installed libraries (#7042)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-11T11:09:34-06:00",
          "tree_id": "ca20b457b55ac8c28d0200652724b81ba90d17f7",
          "url": "https://github.com/ocaml/dune/commit/7cc9523524450417f306b5d3f4a1acbd39d88472"
        },
        "date": 1676136515687,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.433761064753334",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "60a36f07c790402beb5f2e42db25b390f9ff0aba",
          "message": "refactor: avoid Path.as_outside_build_dir_exn (#6998)\n\nUse Path.Outside_build_dir.t to represent workspace paths\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-11T12:26:49-06:00",
          "tree_id": "b8ef9a59e3d3be546626776c6b57cfc323b9e1b5",
          "url": "https://github.com/ocaml/dune/commit/60a36f07c790402beb5f2e42db25b390f9ff0aba"
        },
        "date": 1676141157071,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.24336486029333",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "d6977f2e67d29b30d04fd4866c81596cbecd653e",
          "message": "test: test action_to_sh outputs (#7039)\n\nWe test the outputs of Action_to_sh. The list follows the actions\r\ndeclared in the user manual.\r\n\r\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-02-11T13:29:00-06:00",
          "tree_id": "70479896175a0c26a62d4a09d1c00e338c422b13",
          "url": "https://github.com/ocaml/dune/commit/d6977f2e67d29b30d04fd4866c81596cbecd653e"
        },
        "date": 1676144874887,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.87096662458",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "11337d8c5e1cd1baed7aa0b90176d13c8dfc0752",
          "message": "refactor(cache): replace fold with iter (#7044)\n\nThe [fold] is done with a unit accumulate, so it's exactly equivalent to\r\n[iter]\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-11T22:59:39-06:00",
          "tree_id": "71bbb5640a8c702263a2b729b835092115e90be0",
          "url": "https://github.com/ocaml/dune/commit/11337d8c5e1cd1baed7aa0b90176d13c8dfc0752"
        },
        "date": 1676179691165,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.856256833859995",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "2db9840dfe45089d8fc0c61acf887571641dc5a7",
          "message": "test(rules): (include_subdirs qualified) and installed paths (#7050)\n\ndemonstrate the installed paths inside the dune-package file\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-12T09:39:20-06:00",
          "tree_id": "d6e42da920de3525745f32f538fb2b9acc872940",
          "url": "https://github.com/ocaml/dune/commit/2db9840dfe45089d8fc0c61acf887571641dc5a7"
        },
        "date": 1676217471707,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.46148003369333",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "ilankri@protonmail.com",
            "name": "Idir Lankri",
            "username": "ilankri"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "b9c27038fbf0d69e173e5f045b688d00eb47796b",
          "message": "feature(emacs): auto-detect `dune-workspace` files as `dune` files (#7061)\n\nSigned-off-by: Idir Lankri <ilankri@protonmail.com>",
          "timestamp": "2023-02-13T09:56:43-06:00",
          "tree_id": "df4fdaaae483cb19d27543a4d76af10df30c1ba3",
          "url": "https://github.com/ocaml/dune/commit/b9c27038fbf0d69e173e5f045b688d00eb47796b"
        },
        "date": 1676305064168,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "38.8039667948",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "committer": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "20e0b2670f0f7c69579d2b89ed3ec3f413a756c9",
          "message": "add changelog\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-02-13T21:59:28+01:00",
          "tree_id": "6c4e4490b0924fa04f85f26e981e8fd4f5cc9ee5",
          "url": "https://github.com/ocaml/dune/commit/20e0b2670f0f7c69579d2b89ed3ec3f413a756c9"
        },
        "date": 1676323207299,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "40.74426305168",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "committer": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "0023fe0603a8e4d556500024d68c9bfc8dc79a0b",
          "message": "chore(coq): cleanup internal error about duplicate keys for dep map\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>\n\n<!-- ps-id: 8edcef02-9f8e-4bf9-bd79-9bad40b9c503 -->",
          "timestamp": "2023-02-13T22:49:24+01:00",
          "tree_id": "84a3669356c1cf1d035a3b6ab74c22975992dbc0",
          "url": "https://github.com/ocaml/dune/commit/0023fe0603a8e4d556500024d68c9bfc8dc79a0b"
        },
        "date": 1676326064424,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.35181666634001",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "committer": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "076cfe8220e01e926618bcbf229ddc515e516b43",
          "message": "test(coq): add test for empty modules field\n\nCurrently raises an internal error.\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>\n\n<!-- ps-id: 321899da-3c3c-4be4-9b2f-3c5afe19b4ca -->",
          "timestamp": "2023-02-13T23:10:42+01:00",
          "tree_id": "4d3b3eec3711ff87dbebee69d9cc83fca21c2839",
          "url": "https://github.com/ocaml/dune/commit/076cfe8220e01e926618bcbf229ddc515e516b43"
        },
        "date": 1676327378731,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.81510958373334",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "e+git@x80.org",
            "name": "Emilio Jesus Gallego Arias",
            "username": "ejgallego"
          },
          "committer": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "4438f445a2060a5f3715c18d1e16bb2b804bf437",
          "message": "[coq] Hide implementation of dependency maps\n\nThis allow us to share the error codepath, plus provides a much\ncleaner setup IMO.\n\nSigned-off-by: Emilio Jesus Gallego Arias <e+git@x80.org>",
          "timestamp": "2023-02-13T23:14:34+01:00",
          "tree_id": "92e44d7e0a425a13633ddae162b3fe910adb6831",
          "url": "https://github.com/ocaml/dune/commit/4438f445a2060a5f3715c18d1e16bb2b804bf437"
        },
        "date": 1676327614943,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.007634663013334",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "e+git@x80.org",
            "name": "Emilio Jesus Gallego Arias",
            "username": "ejgallego"
          },
          "committer": {
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "f5d7b12d8084b7203d7ef5e5cd0c7c2376506b88",
          "message": "[coq] [coqdep] Better error handling, inject prelude dep in post-process\n\nSome further tweaks, in particular we handle errors better, and inject\nthe prelude dependency at module dep time, instead of when building\nthe dep map, which is the wrong phase.\n\nThis will allow us to not to need boot_type to call coqdep when\n`-boot` becomes the default in the installed_theories PR.\n\nSigned-off-by: Emilio Jesus Gallego Arias <e+git@x80.org>",
          "timestamp": "2023-02-14T00:02:18+01:00",
          "tree_id": "cead4c1a8dd61f7678aa14e578769e2515139797",
          "url": "https://github.com/ocaml/dune/commit/f5d7b12d8084b7203d7ef5e5cd0c7c2376506b88"
        },
        "date": 1676330623538,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "40.283156469106665",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "ec583d27d2904b7d837cde30c9497f1d6a7ff3d6",
          "message": "fix(rules): installed paths (#7063)\n\nWhen relocating modules from regular paths (relative to where they are\r\ndefined) to installed paths (relative to _build/install), correctly take\r\ninto account (include_subdirs ..). Previously, we would flatten the\r\nentire directory structure in the installed directory. Now we preserve\r\nit upto the the library root.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-13T18:39:34-06:00",
          "tree_id": "813f48a801bb5bda72dfd55d804128b5d5d24043",
          "url": "https://github.com/ocaml/dune/commit/ec583d27d2904b7d837cde30c9497f1d6a7ff3d6"
        },
        "date": 1676336310175,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.38793588716667",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "31816d06dcfdb71fbb63a048c3c6fbf98cbb714a",
          "message": "refactor(rules): explicit import list (#7074)\n\nUse explicit import list from engine.\r\n\r\nThere's some modules we don't use in the rules so there's no need to\r\nbring them in scope.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-14T09:16:45-06:00",
          "tree_id": "18d170ef362ffe5b1d315ba9f078797282ec3939",
          "url": "https://github.com/ocaml/dune/commit/31816d06dcfdb71fbb63a048c3c6fbf98cbb714a"
        },
        "date": 1676388969677,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "37.07538513058",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "9328d9d7d07e6f007a416513829c7b1736f0f364",
          "message": "refactor(rules): remove debug_findlib flag (#7076)\n\nIt doesn't do anything anymore\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-14T09:16:03-06:00",
          "tree_id": "cb940d83a1f0e4c0242c16d400e02ad102686ac0",
          "url": "https://github.com/ocaml/dune/commit/9328d9d7d07e6f007a416513829c7b1736f0f364"
        },
        "date": 1676389043791,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "37.79860549442666",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "4bc0b8e35e19cd81db8d732d313623c8cdc865ca",
          "message": "refactor: remove no_print_directory from global flags (#7078)\n\nit's only used in one place so it doesn't need to be global\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-14T10:20:54-06:00",
          "tree_id": "523bc913edd8191fa6b59736f9e0847590dc8cf3",
          "url": "https://github.com/ocaml/dune/commit/4bc0b8e35e19cd81db8d732d313623c8cdc865ca"
        },
        "date": 1676392825102,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.49752110791332",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "1d07478a0247426f6d0cc3cbf0220126ec2b5da5",
          "message": "chore: remove notty (#7082)\n\nThere was a leftover dune file from notty\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-14T11:45:00-06:00",
          "tree_id": "b629fa370eb2d8d3f3a21521cc1d80fd9d509ce4",
          "url": "https://github.com/ocaml/dune/commit/1d07478a0247426f6d0cc3cbf0220126ec2b5da5"
        },
        "date": 1676398100219,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "41.950342651486665",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "5fdaa96753ceb5eb9e00064ae38a22cc61c4d405",
          "message": "refactor: replace (fun x -> x) (#7083)\n\nReplace it with [Fun.id] from the [Stdlib]\r\n\r\nAlso replace [List.filter_map] and [List.filter_opt] when [~f] is\r\n[Fun.id]\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-14T17:34:26-06:00",
          "tree_id": "b85ae9a765b7a9c6a06819060d584f8d7b9bdfd1",
          "url": "https://github.com/ocaml/dune/commit/5fdaa96753ceb5eb9e00064ae38a22cc61c4d405"
        },
        "date": 1676418781034,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.52180888878",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "e4c5974e38a24c72c48e78d20803b8e9e4b6c215",
          "message": "test: top closure with mli only (#7081)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-14T19:49:45-06:00",
          "tree_id": "475c66806e77016df78192f9b30e73ce75824666",
          "url": "https://github.com/ocaml/dune/commit/e4c5974e38a24c72c48e78d20803b8e9e4b6c215"
        },
        "date": 1676426939800,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "36.72426433770667",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "n.oje.bar@gmail.com",
            "name": "Nicolás Ojeda Bär",
            "username": "nojb"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "73506c2ed639d361ec406b31a917202160b6518c",
          "message": "Add native polling mode support on Windows (#7010)\n\nCo-authored-by: Uma Kothuri <uma@kothuri.net>\r\nSigned-off-by: Uma Kothuri <uma@kothuri.net>\r\nSigned-off-by: nojebar <nicolas.ojeda.bar@lexifi.com>",
          "timestamp": "2023-02-15T12:19:32+01:00",
          "tree_id": "2ef27bfcd4bc2434cebc96a7d288d8bc2c95432b",
          "url": "https://github.com/ocaml/dune/commit/73506c2ed639d361ec406b31a917202160b6518c"
        },
        "date": 1676461100986,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.642917211240004",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "n.oje.bar@gmail.com",
            "name": "Nicolás Ojeda Bär",
            "username": "nojb"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "2a1f900d08108ab2644ae239e184bce08880cbb0",
          "message": "Fix build (#7089)\n\nSigned-off-by: nojebar <nicolas.ojeda.bar@lexifi.com>",
          "timestamp": "2023-02-15T16:36:14+01:00",
          "tree_id": "d4d03b8bb0f4388f04c7c7041e3b05ea6567f204",
          "url": "https://github.com/ocaml/dune/commit/2a1f900d08108ab2644ae239e184bce08880cbb0"
        },
        "date": 1676476532422,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "37.239364818826665",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "ab207b017bd1d54733d9c34564fd3e12782051ae",
          "message": "test: virtual libraries bug (#7085)\n\nreproduce a bug where an implementation of a virtual library sometimes\r\ndoesn't include the virtual module's implementation\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-15T11:40:57-06:00",
          "tree_id": "66c0291548eb10078c260a322fd71281426b2eb6",
          "url": "https://github.com/ocaml/dune/commit/ab207b017bd1d54733d9c34564fd3e12782051ae"
        },
        "date": 1676483978274,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.987308447273335",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "f1ff192613b0cdfcdb09043b28b0fed4885da38c",
          "message": "refactor(rules): move dep logic to dep rules (#7084)\n\nDetails such as singleton modules are implementation details of the\r\nrules. So we move them there from the dune cli.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-15T11:48:44-06:00",
          "tree_id": "ca14384de7f18e8c1f420e664d75dbd811302716",
          "url": "https://github.com/ocaml/dune/commit/f1ff192613b0cdfcdb09043b28b0fed4885da38c"
        },
        "date": 1676484455497,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.909920567953336",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "marek@tarides.com",
            "name": "Marek Kubica",
            "username": "Leonidas-from-XIV"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "7ddf2cd85c9f0e6f515954069c0c1edb50f55409",
          "message": "Fix minor typos, add clarifications to OPAM generation (#7090)\n\nSigned-off-by: Marek Kubica <marek@tarides.com>",
          "timestamp": "2023-02-15T16:44:05-06:00",
          "tree_id": "838ce25fa1f9f3c2690226708e7d94d6a3be98c0",
          "url": "https://github.com/ocaml/dune/commit/7ddf2cd85c9f0e6f515954069c0c1edb50f55409"
        },
        "date": 1676502449210,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "37.322798538853334",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "1f463af9bc20dc9c04741d08ddb297cca3859094",
          "message": "refactor: dep handling (#7094)\n\nsimplify ocamldep rules with better names and |>\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-15T18:38:52-06:00",
          "tree_id": "f82053ae22302e4e13646d67779071486e186b25",
          "url": "https://github.com/ocaml/dune/commit/1f463af9bc20dc9c04741d08ddb297cca3859094"
        },
        "date": 1676509266156,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "42.723080276813334",
            "unit": "seconds"
          }
        ]
      }
    ]
  }
}