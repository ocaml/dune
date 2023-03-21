window.BENCHMARK_DATA = {
  "lastUpdate": 1679361472935,
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
          "id": "a23bfc40f5643bd9970e9c2a407e6cf802be7bc5",
          "message": "refactor: simplify Top_closure (#7093)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-15T18:46:33-06:00",
          "tree_id": "6e0718f2984226ffd89aed5438b3be59cb33261f",
          "url": "https://github.com/ocaml/dune/commit/a23bfc40f5643bd9970e9c2a407e6cf802be7bc5"
        },
        "date": 1676509493122,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.37725012683333",
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
          "id": "ec504c6f452681c7ce4e197def73c1d72e1a18a6",
          "message": "Add support for eager watch mode for `dune exec` (#6966)\n\nThis is the second attempt at adding this feature. The problem with the\r\nfirst attempt was that replacing the call to `Scheduler.go` with\r\n`Scheduler.go_with_rpc_server_and_console_status_reporting` caused\r\noccasional non-deterministic seg faults on macos. There's some info\r\nabout the problem on the PR which reverts the previous attempt at adding\r\nthis feature: https://github.com/ocaml/dune/pull/6867. At the time of\r\nwriting we don't know the cause of this problem.\r\n\r\nThe difference this time around is that we maintain the call to\r\n`Scheduler.go` when running `dune exec` not in watch mode, and only\r\ninvoke `Scheduler.go_with_rpc_server_and_console_status_reporting` when\r\nrunning in watch mode. There is some additional refactoring done to make\r\nthis split more ergonomic. We may see seg faults on macos when running\r\nexec in watch mode but at least we won't introduce the potential for seg\r\nfaults into the existing use case of running exec not in watch mode\r\n(assuming of course that there is a causal link between\r\n`go_with_rpc_server_and_console_status_reporting` and the seg fault on\r\nmacos, which is not necessarily the case).\r\n\r\nSigned-off-by: Stephen Sherratt <stephen@sherra.tt>",
          "timestamp": "2023-02-15T22:30:35-06:00",
          "tree_id": "5dd85f8969653db48be51c4df44ed782b7fe8574",
          "url": "https://github.com/ocaml/dune/commit/ec504c6f452681c7ce4e197def73c1d72e1a18a6"
        },
        "date": 1676523189131,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "42.296149648286665",
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
          "id": "541c858c9d8674d2205af0b725cac3df4787337d",
          "message": "chore: changes entry for dune exec -w (#7097)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-15T22:38:29-06:00",
          "tree_id": "afca0b5a06084ec86511b02d456aff20eeb5fd7c",
          "url": "https://github.com/ocaml/dune/commit/541c858c9d8674d2205af0b725cac3df4787337d"
        },
        "date": 1676523436039,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.60853389201333",
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
          "id": "05797cd3f505beca4d6d62c576506d7608a6d29d",
          "message": "test(stdune): add top closure tests (#7095)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-15T23:08:10-06:00",
          "tree_id": "8c945aefd28796bf87a0e0312dd8c900056db514",
          "url": "https://github.com/ocaml/dune/commit/05797cd3f505beca4d6d62c576506d7608a6d29d"
        },
        "date": 1676525202298,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.409556729200006",
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
          "id": "e6a8490a4745bd6405056adecf2bb2639e230c77",
          "message": "Fix typos in 3.7 changelog (#7099)\n\nSigned-off-by: Etienne Millon <me@emillon.org>",
          "timestamp": "2023-02-17T14:45:49+01:00",
          "tree_id": "e6a7d0471666c8652d2bb04b39d950a697678f91",
          "url": "https://github.com/ocaml/dune/commit/e6a8490a4745bd6405056adecf2bb2639e230c77"
        },
        "date": 1676642663605,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.491347689499996",
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
          "id": "c58486fa0c56e5f633413a08892bbb3df4412ac9",
          "message": "fix: multiple virtual modules per lib (#7092)\n\nFix an incorrectly computed module closure for virtual libraries with\r\nmore than one module.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>\r\n\r\n<!-- ps-id: a700deba-e52f-4402-8430-8b21dfcfc2aa -->",
          "timestamp": "2023-02-17T15:20:43+01:00",
          "tree_id": "d471e5298f32b272e3a77ece1905ac41095e65b4",
          "url": "https://github.com/ocaml/dune/commit/c58486fa0c56e5f633413a08892bbb3df4412ac9"
        },
        "date": 1676644798348,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.77588982742667",
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
          "id": "e4a1083ebae61c47283333010d3e475ddc2e6eac",
          "message": "fix(melange): installed libraries path (#7072)\n\nPreserve .js paths of melange artifacts upto the source directory of the\r\nlibrary\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>\r\n\r\n<!-- ps-id: 8f5ad532-672d-440d-9730-0a0fff09767f -->",
          "timestamp": "2023-02-17T15:39:01+01:00",
          "tree_id": "2c0334a1c7ab02e5a041da0945d30ab8096dc0ca",
          "url": "https://github.com/ocaml/dune/commit/e4a1083ebae61c47283333010d3e475ddc2e6eac"
        },
        "date": 1676645853937,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "32.89944172942666",
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
          "id": "d3d628f2eda2278bd2df6e37452d8693f367fcfd",
          "message": "Prepare 3.7.0 (#7105)\n\nSigned-off-by: Etienne Millon <me@emillon.org>",
          "timestamp": "2023-02-17T15:44:16+01:00",
          "tree_id": "b75e29410537e1be7383b7841616821f08d83161",
          "url": "https://github.com/ocaml/dune/commit/d3d628f2eda2278bd2df6e37452d8693f367fcfd"
        },
        "date": 1676646161406,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.25559528527334",
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
          "id": "e8853cc3258964d60f00984c851bf1995fd8cb4e",
          "message": "refactor(engine): fetching execution parameters (#7002)\n\nMove the fetching of execution parameters to the rules.\r\n\r\nCurrently, the execution parameters are obtained from the dune-project\r\nfile. Therefore, this is part of the frontend and not the engine.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-18T23:54:54-06:00",
          "tree_id": "da2ad99ba22adfeb2b97b4d1b07e887d3d8c18e4",
          "url": "https://github.com/ocaml/dune/commit/e8853cc3258964d60f00984c851bf1995fd8cb4e"
        },
        "date": 1676787849959,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.270896756073334",
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
          "id": "20a1f1023b04284ad12b0a83fdf57a6ae4faa354",
          "message": "chore: bump Dune version to 3.8 (#7051)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-02-19T00:13:37-06:00",
          "tree_id": "564c54b333446d028cda29053164d5029a0c2c1b",
          "url": "https://github.com/ocaml/dune/commit/20a1f1023b04284ad12b0a83fdf57a6ae4faa354"
        },
        "date": 1676788797633,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.454153597006666",
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
          "id": "4a9d0dbb04ae6e1e36e6437f20c157328cf07eb8",
          "message": "chore(coq): bump coq lang to 0.8\n\n<!-- ps-id: 501a4787-9f2f-4fba-9e59-f1ddd56dc938 -->\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-02-19T14:53:16Z",
          "tree_id": "95bcc591af206959be731778314e17c14a61f9ed",
          "url": "https://github.com/ocaml/dune/commit/4a9d0dbb04ae6e1e36e6437f20c157328cf07eb8"
        },
        "date": 1676819705699,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "39.62903697279333",
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
          "id": "f8a648c7f5a2e89bb56f0808fd5c4ed6e5f0feb7",
          "message": "coq: hide .theory.d files with dot prefix\n\n<!-- ps-id: fe701320-95ab-49e5-ad1c-64cd876d1a39 -->\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-02-20T03:30:59+01:00",
          "tree_id": "a61c96c9c2812351f71cfb552c58f0be4ae4cf5d",
          "url": "https://github.com/ocaml/dune/commit/f8a648c7f5a2e89bb56f0808fd5c4ed6e5f0feb7"
        },
        "date": 1676861499809,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "37.92864760807333",
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
          "id": "c5994e2be7e2048fc65ea0101d9b89709fff775f",
          "message": "refactor: move OCaml modules to Dialect (#7118)\n\nThey shouldn't be visible to the entire engine\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-19T21:08:51-06:00",
          "tree_id": "0379d0f33ec51902a46a9b0dd26518cff8dd6a04",
          "url": "https://github.com/ocaml/dune/commit/c5994e2be7e2048fc65ea0101d9b89709fff775f"
        },
        "date": 1676863669453,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.890543331180005",
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
          "id": "e34fa414986b9a08d6a13e8ae8604e718db5b644",
          "message": "refactor(engine): remove unneeded deps (#7117)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-19T21:09:47-06:00",
          "tree_id": "766c5b5e671d7211a19c3d3c86f8fb85c3bd133e",
          "url": "https://github.com/ocaml/dune/commit/e34fa414986b9a08d6a13e8ae8604e718db5b644"
        },
        "date": 1676863723816,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.72842637937333",
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
          "id": "f30803b7fcb902eb313eef6e2ccdd5e62da61e50",
          "message": "fix(mdx): record runtime dependency to prelude (#7109)\n\nWhen mdx is used in program-generation mode (version >= 0.2), the paths\r\nused as preludes are recorded in the executable, but the contents of\r\nfiles are only read when the program is executed. This adds the missing\r\ndependencies.\r\n\r\nFixes #7077\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>",
          "timestamp": "2023-02-20T16:38:46+01:00",
          "tree_id": "34ef4cde87acb3a644160cca4b964195ca48a84a",
          "url": "https://github.com/ocaml/dune/commit/f30803b7fcb902eb313eef6e2ccdd5e62da61e50"
        },
        "date": 1676908837637,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "40.37613453791334",
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
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "distinct": true,
          "id": "b6d28723c8edbc8a5829e8502bb6b45956933151",
          "message": "test: preprocessor directory\n\nSigned-off-by: Stephen Sherratt <stephen@sherra.tt>",
          "timestamp": "2023-02-20T16:45:20-08:00",
          "tree_id": "b4703350652dfd13e9414c5a0a6d1682898e40d4",
          "url": "https://github.com/ocaml/dune/commit/b6d28723c8edbc8a5829e8502bb6b45956933151"
        },
        "date": 1676942058695,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.068237062646666",
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
          "id": "0c287fa555d1ded452a3bde5de65f69257ea8056",
          "message": "fix: error message for \"too many arguments\" (#7134\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-02-20T16:46:19-08:00",
          "tree_id": "6b43c5a6c202a7d4ee10b10b0ce499cef223a87a",
          "url": "https://github.com/ocaml/dune/commit/0c287fa555d1ded452a3bde5de65f69257ea8056"
        },
        "date": 1676942122000,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.6133205117",
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
          "id": "da389a59f3896e297b9ba3f4b4e68b9b38526203",
          "message": "test: data_only_dirs and aliases (#7129\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-20T17:46:02-08:00",
          "tree_id": "ea5fb207b512279bc14f1c17b7e0906aa3655496",
          "url": "https://github.com/ocaml/dune/commit/da389a59f3896e297b9ba3f4b4e68b9b38526203"
        },
        "date": 1676945064152,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "32.89702668886667",
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
          "id": "8d552d3064644da62347d4afc8db88ab27452fa5",
          "message": "refactor(console): split Dune_console.Backend (#7000\n\nMove the dumb and progress backends to their own files.\r\n\r\nMove flushing/composition to a [Combinators] module\r\n\r\nFinally, [Console.Backend.progress] is now flushing. Previously, it\r\nwouldn't flush and was therefore buggy. This didn't affect dune, as we\r\nonly used [progress_threaded].\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-20T17:49:45-08:00",
          "tree_id": "33096efed9fa269e301165ab5ce9d3642ed303f1",
          "url": "https://github.com/ocaml/dune/commit/8d552d3064644da62347d4afc8db88ab27452fa5"
        },
        "date": 1676945372573,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "37.45189395579334",
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
          "id": "65802a9783e5d8e46db0388560bf2fef50a773aa",
          "message": "refactor: remove [Rule.find_source_dir] (#7103\n\nIt's not used anywhere and it depends on [Source_tree.nearest_dir]\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-20T18:37:54-08:00",
          "tree_id": "02053e005342314a05cfd16c39da833fd07cdd31",
          "url": "https://github.com/ocaml/dune/commit/65802a9783e5d8e46db0388560bf2fef50a773aa"
        },
        "date": 1676948200870,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.486056002119994",
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
          "id": "a6e36414758d24c600fefc627e389426a399004a",
          "message": "refactor: move rule printing (#7101\n\nRule printing is only needed for a single sub command (dune rules).\r\nTherefore, traversing the rules can done directly in the cli instead of\r\nthe engine.\r\n\r\nThis removes a source tree traversal from the engine and reduces the\r\ndependency between the engine and the source tree.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-20T18:39:04-08:00",
          "tree_id": "1ff2269eac56f16678c344cd686d0672639a9335",
          "url": "https://github.com/ocaml/dune/commit/a6e36414758d24c600fefc627e389426a399004a"
        },
        "date": 1676948482259,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "40.67768286754",
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
          "id": "b62e8a1b65466e83bfe0ee8631565e07d4a6bf99",
          "message": "refactor: style adjustments (#7128)",
          "timestamp": "2023-02-20T18:44:47-08:00",
          "tree_id": "b7009e2126b51f39100a1b5f039e6fc35aa8fe38",
          "url": "https://github.com/ocaml/dune/commit/b62e8a1b65466e83bfe0ee8631565e07d4a6bf99"
        },
        "date": 1676948926722,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "42.645030828033335",
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
            "email": "e+git@x80.org",
            "name": "Emilio Jesús Gallego Arias",
            "username": "ejgallego"
          },
          "distinct": true,
          "id": "43ef3dcd55cf0497b9f4b2c916bc5c27aba6c609",
          "message": "[coq] Refactor Coq_lib creation\n\nThere's some strange stuff going on here with boot, which we want to\nhandle better; so we do some cleanup and logical splitting of components.\n\nSigned-off-by: Emilio Jesus Gallego Arias <e+git@x80.org>",
          "timestamp": "2023-02-21T04:08:27+01:00",
          "tree_id": "ecbc7eda9fe51f8459faa3623c0d686b21027861",
          "url": "https://github.com/ocaml/dune/commit/43ef3dcd55cf0497b9f4b2c916bc5c27aba6c609"
        },
        "date": 1676950059347,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "36.73901771858666",
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
          "id": "4edb5ff19aa3efcabdc8194ef7f837e49866517b",
          "message": "refactor: move some flags to rules (#7080\n\nSome flags are only used in the rules, so they should live there instead\r\nof the engine.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-20T19:51:20-08:00",
          "tree_id": "1111c9b68de3fe7771a19ef4503e29798b40b511",
          "url": "https://github.com/ocaml/dune/commit/4edb5ff19aa3efcabdc8194ef7f837e49866517b"
        },
        "date": 1676952622280,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.07658814395333",
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
          "id": "8c627ef7cb8c14d3f5eb1b50484de81678fd5106",
          "message": "refactor(engine): fetching execution parameters (#7127\n\nMove the fetching of execution parameters to the rules.\r\n\r\nCurrently, the execution parameters are obtained from the dune-project\r\nfile. Therefore, this is part of the frontend and not the engine.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-20T20:06:20-08:00",
          "tree_id": "339f1a734a5350202d1fcb02571cef6c8bb1c7c0",
          "url": "https://github.com/ocaml/dune/commit/8c627ef7cb8c14d3f5eb1b50484de81678fd5106"
        },
        "date": 1676953520919,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.09643306269333",
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
          "id": "364156058b96ada04f8c527159bb4e01bbff6136",
          "message": "refactor: generic source tree (#7115\n\nMake the source tree generic in [Load_rules]\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-20T21:32:39-08:00",
          "tree_id": "3cce6d817874fe18093b811a2b1c22e3ae30cc03",
          "url": "https://github.com/ocaml/dune/commit/364156058b96ada04f8c527159bb4e01bbff6136"
        },
        "date": 1676958868531,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "41.125293628053335",
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
          "id": "c41993f7c03878017c5fe390117a013d79795f3d",
          "message": "test: disable test failing in CI (#7138\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-20T21:53:50-08:00",
          "tree_id": "56c156da555721b59789c8ececc511528937ac0c",
          "url": "https://github.com/ocaml/dune/commit/c41993f7c03878017c5fe390117a013d79795f3d"
        },
        "date": 1676960000067,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "37.773403644586665",
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
          "id": "042e48aa56ed9cb47a559dc985424ec31346d72b",
          "message": "fix(doc): use sphinx < 6 (#7126)\n\nFixes #7107\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>",
          "timestamp": "2023-02-21T09:50:59+01:00",
          "tree_id": "cf493f2c322a4cb5e7bcc8fe827e0903e65716b0",
          "url": "https://github.com/ocaml/dune/commit/042e48aa56ed9cb47a559dc985424ec31346d72b"
        },
        "date": 1676970619096,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.44287951205333",
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
          "id": "96b42793c42530b82417d25769db2553620c82a2",
          "message": "(env): add (bin_annot <bool>) (#7102)\n\nSigned-off-by: Nicolás Ojeda Bär <n.oje.bar@gmail.com>",
          "timestamp": "2023-02-21T11:43:17+01:00",
          "tree_id": "3195614ad1cfddd317815c8d376f0482adebfefb",
          "url": "https://github.com/ocaml/dune/commit/96b42793c42530b82417d25769db2553620c82a2"
        },
        "date": 1676977319146,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.19685672097334",
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
          "id": "bf609bafc8198b8f54ff95082ed9c452d2054542",
          "message": "fix(console): Make [Backend.progress] flush (#7141\n\nSome time ago, it was accidentally changed not to flush. The change was\r\nnever noticed because dune used the threaded version of this backend\r\nthat did flush.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-21T06:48:38-08:00",
          "tree_id": "3a788bf915d8b69f97833409d6ff68c03d2a99b7",
          "url": "https://github.com/ocaml/dune/commit/bf609bafc8198b8f54ff95082ed9c452d2054542"
        },
        "date": 1676992638333,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "36.939666100859995",
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
          "id": "c0b3219779766c65dc0e17a8881a1fc5da0091f1",
          "message": "test: fix sed'ing a symlink (#7140\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-21T06:50:22-08:00",
          "tree_id": "524c62ff40d4d59d9c787b86e42b1992d586baa1",
          "url": "https://github.com/ocaml/dune/commit/c0b3219779766c65dc0e17a8881a1fc5da0091f1"
        },
        "date": 1676992783439,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.37781489655334",
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
          "id": "2f9cd7390939d1dddd5ef64761c0bc81d54ea402",
          "message": "Add a new line at the end of error reports (#6823\n\nWhenever --display-separate-messages is passed, dune will separate error\r\nmessages by blank lines\r\n\r\nSigned-off-by: Benoît Montagu <benoit.montagu@inria.fr>",
          "timestamp": "2023-02-21T06:48:06-08:00",
          "tree_id": "1b8d024d8708336209cdc3cedd3590d8a4ae8493",
          "url": "https://github.com/ocaml/dune/commit/2f9cd7390939d1dddd5ef64761c0bc81d54ea402"
        },
        "date": 1676992897077,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "43.400245702313335",
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
          "id": "24b31d7f823296fc0b482fde8e8093780d66ab5b",
          "message": "feature: make `modes` field an Ordered_set_lang instance (#6611)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-02-21T08:33:41-08:00",
          "tree_id": "11e547cdcf9f3c39f442e460fc6468a864d4a6e7",
          "url": "https://github.com/ocaml/dune/commit/24b31d7f823296fc0b482fde8e8093780d66ab5b"
        },
        "date": 1676998582219,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "41.83355893240667",
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
          "id": "bfc0047645972bcbba3a45b4b76a5fb3c3e0eec1",
          "message": "refactor(engine): watcher style fixes (#7145)\n\nremove some unnecessary variables and closure allocations\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-21T10:27:46-08:00",
          "tree_id": "dc8b4527303fa89a8e95090acfa2171af7ab94fb",
          "url": "https://github.com/ocaml/dune/commit/bfc0047645972bcbba3a45b4b76a5fb3c3e0eec1"
        },
        "date": 1677005174829,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.52484709463334",
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
          "id": "5463cd87c573c28c71d513fbecd8cb17dbdb27b7",
          "message": "feature: Silence display in non-user processes (#6994)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-02-21T11:13:08-08:00",
          "tree_id": "740d502ca74896b5105137c7f3aff5a8df0fc4a7",
          "url": "https://github.com/ocaml/dune/commit/5463cd87c573c28c71d513fbecd8cb17dbdb27b7"
        },
        "date": 1677008237163,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "42.10321208702666",
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
            "email": "e+git@x80.org",
            "name": "Emilio Jesús Gallego Arias",
            "username": "ejgallego"
          },
          "distinct": true,
          "id": "b06584df02043e81a084f97a42504a76bf925a1f",
          "message": "[scope] [coq] Refactoring towards a Coq scope\n\nSigned-off-by: Emilio Jesus Gallego Arias <e+git@x80.org>",
          "timestamp": "2023-02-21T20:50:34+01:00",
          "tree_id": "5e455160bcfeafb6a62f528517e7968d51452bd1",
          "url": "https://github.com/ocaml/dune/commit/b06584df02043e81a084f97a42504a76bf925a1f"
        },
        "date": 1677010375080,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "40.15041306598",
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
          "id": "f5a5a9c17be60b1cf2ec22fee35d35ccc3bb5e5e",
          "message": "refactor: remove [Action_builder.source_tree] (#7131)",
          "timestamp": "2023-02-21T13:23:16-08:00",
          "tree_id": "1ae6d9f474bb72d0af81f4e5393d347e628a5e85",
          "url": "https://github.com/ocaml/dune/commit/f5a5a9c17be60b1cf2ec22fee35d35ccc3bb5e5e"
        },
        "date": 1677015775719,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "37.076240548293335",
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
            "email": "e+git@x80.org",
            "name": "Emilio Jesús Gallego Arias",
            "username": "ejgallego"
          },
          "distinct": true,
          "id": "d6faeac7e9c35cf9e1f53660b12d90631465bbda",
          "message": "[coq lib] Resolve boot as a regular theory\n\nThis simplifies the code quite a bit, and should help further\nrefactoring for installed_theories.\n\nIn particular we bring a simpliciation in terms of the boot lib, we\njust store its name and use the regular resolve path for it.\n\nThat avoids the hack we had with a forward ref for having it resolve\n\"early\".\n\nSigned-off-by: Emilio Jesus Gallego Arias <e+git@x80.org>",
          "timestamp": "2023-02-22T03:36:17+01:00",
          "tree_id": "b63c36b2ef0a123803580611fc882f7cf0b3f4ae",
          "url": "https://github.com/ocaml/dune/commit/d6faeac7e9c35cf9e1f53660b12d90631465bbda"
        },
        "date": 1677034512565,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.25137180936667",
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
          "id": "5524321b0b5da606521bde063d9909eea31e3fab",
          "message": "Enable use of stublibs of Dune-installed libraries from within workspace (#7151)\n\nSigned-off-by: Nicolás Ojeda Bär <n.oje.bar@gmail.com>",
          "timestamp": "2023-02-22T13:52:21+01:00",
          "tree_id": "50a319c3279d7338a2155b1bb506c8f6e43af78d",
          "url": "https://github.com/ocaml/dune/commit/5524321b0b5da606521bde063d9909eea31e3fab"
        },
        "date": 1677071462486,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.42559987176667",
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
          "id": "a991faf36219d92f434543b5d1fff080a89c4311",
          "message": "Repro for #7146: Linker is invoked from unexpected directory (#7147)\n\nSigned-off-by: Stephen Sherratt <stephen@sherra.tt>",
          "timestamp": "2023-02-22T06:21:24-08:00",
          "tree_id": "daffff2fb10b545de73fa444123c5f6da2fba2de",
          "url": "https://github.com/ocaml/dune/commit/a991faf36219d92f434543b5d1fff080a89c4311"
        },
        "date": 1677076867893,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.94181495206",
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
          "id": "486c18acb55d4adcc4d507048e9070d2bd3c8415",
          "message": "group describe tests in the same folder\n\nSigned-off-by: Corentin Leruth <corentin.leruth@gmail.com>",
          "timestamp": "2023-02-22T06:32:53-08:00",
          "tree_id": "f15698f8e7870843dbc18b4857e0d2c491915572",
          "url": "https://github.com/ocaml/dune/commit/486c18acb55d4adcc4d507048e9070d2bd3c8415"
        },
        "date": 1677077546878,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "38.17791138534667",
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
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "distinct": true,
          "id": "3c3382dce1a4bd6f27c13ca0313dfc3613b6267a",
          "message": "fix: point to the correct melange artifact location in external libs\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-02-22T10:28:47-08:00",
          "tree_id": "eecf5eabd0487be7b00e60979b840a22f826f859",
          "url": "https://github.com/ocaml/dune/commit/3c3382dce1a4bd6f27c13ca0313dfc3613b6267a"
        },
        "date": 1677091937406,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "42.820528003453326",
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
          "id": "d9936f1b6cd436b7b1bfbf2a51ff6754baf6168c",
          "message": "test: demonstrate bug in stdlib decoding in dune-package (#7159)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-02-22T11:15:15-08:00",
          "tree_id": "a6f6b1232ec4f80155a6d357d26344f826446d07",
          "url": "https://github.com/ocaml/dune/commit/d9936f1b6cd436b7b1bfbf2a51ff6754baf6168c"
        },
        "date": 1677094448155,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.66283434735333",
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
          "id": "e7c0a10839f7d9fc5e634158f969679a66966d29",
          "message": "stdlib: include dependencies for stdlib alias, decode correctly (#7154)\n\ninclude dependencies for stdlib alias, decode correctly\r\n\r\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-02-22T11:25:46-08:00",
          "tree_id": "6699cea67ad879bf9c0f986d9a1969921051e2b6",
          "url": "https://github.com/ocaml/dune/commit/e7c0a10839f7d9fc5e634158f969679a66966d29"
        },
        "date": 1677095084342,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "36.63396717928",
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
          "id": "8c02563e67e1f6af1f3dcede3acf751cec28aaa6",
          "message": "test: make github7146 reproducible (#7160)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-22T17:28:53-08:00",
          "tree_id": "3101f92ad0f06b314034509b3101d92d0abcef0c",
          "url": "https://github.com/ocaml/dune/commit/8c02563e67e1f6af1f3dcede3acf751cec28aaa6"
        },
        "date": 1677117538525,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "42.90220344156668",
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
          "id": "cec21681eb0e6b10ddb3bf59b81b04779744f3f1",
          "message": "test: installed stublibs (#7162)\n\nadd some dummy code to the test stub to avoid warnings on macos\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-22T18:06:26-08:00",
          "tree_id": "5d7299e4e75fa163485afac6cbb79fcc2282ace2",
          "url": "https://github.com/ocaml/dune/commit/cec21681eb0e6b10ddb3bf59b81b04779744f3f1"
        },
        "date": 1677119088363,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.04277709404",
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
          "id": "20180d12149343d073cdea5860d01dc181702e6a",
          "message": "refactor: move rpc client to own library (#7120)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-22T19:50:30-08:00",
          "tree_id": "a2705f2ea4b83c44aa52d0e7830080d567de216c",
          "url": "https://github.com/ocaml/dune/commit/20180d12149343d073cdea5860d01dc181702e6a"
        },
        "date": 1677125376106,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.02356362868",
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
          "id": "6b998ea289d42437d28267bac20a0abb3ca8907d",
          "message": "bc-for-jsoo: use -noautolink, do not depend on stublibs (#7156)\n\nSigned-off-by: Nicolás Ojeda Bär <n.oje.bar@gmail.com>",
          "timestamp": "2023-02-23T16:05:07+01:00",
          "tree_id": "22efd74050bbef274e6ab89afe22725d71fa0064",
          "url": "https://github.com/ocaml/dune/commit/6b998ea289d42437d28267bac20a0abb3ca8907d"
        },
        "date": 1677165819286,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "32.896837353040006",
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
          "id": "96d4560b0ff362ee4eefbbd3e84d2518afdfe466",
          "message": "chore(ci): update macos deps (#7165)\n\nAdd pkg-config and file as deps\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-23T08:37:12-08:00",
          "tree_id": "1484f5a7eb35ce0f6b196f53c396bafa974d92f8",
          "url": "https://github.com/ocaml/dune/commit/96d4560b0ff362ee4eefbbd3e84d2518afdfe466"
        },
        "date": 1677171510272,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "40.74990790788667",
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
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "distinct": true,
          "id": "8fff50edc333e279429acda5d5ef0ed99537a75b",
          "message": "fix: decode dune-package source paths from the dune-package dir\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-02-23T08:46:07-08:00",
          "tree_id": "ad87c4da7025ed31d2c1a3a8bc7b6d9dfcb27b5d",
          "url": "https://github.com/ocaml/dune/commit/8fff50edc333e279429acda5d5ef0ed99537a75b"
        },
        "date": 1677172218900,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "44.28683251148667",
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
          "id": "676f6c7645f09a61562c0800c88ccfe3f57549f7",
          "message": "fix(melange): unify public libraries (in-workspace vs external) (#7163)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-02-23T09:41:36-08:00",
          "tree_id": "c11ad3670d43b1756bbc223e461270b2e23cd05a",
          "url": "https://github.com/ocaml/dune/commit/676f6c7645f09a61562c0800c88ccfe3f57549f7"
        },
        "date": 1677175245848,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.685160802393334",
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
          "id": "f3bdf371f88acae4d752bc3decc83f43039eab29",
          "message": "chore: missing space in CHANGES (#7168)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-23T09:45:15-08:00",
          "tree_id": "4396348a4905c450b930eaac9b9050853f050569",
          "url": "https://github.com/ocaml/dune/commit/f3bdf371f88acae4d752bc3decc83f43039eab29"
        },
        "date": 1677175976023,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "51.97186776184666",
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
          "id": "28545f3f729d6b203bc3f4f5bd5bda5ba975bb3a",
          "message": "refactor: simplify Dir_contents a bit (#7173)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-23T13:06:24-08:00",
          "tree_id": "febdd41a4a0c84f496427d08a1d70b322b709040",
          "url": "https://github.com/ocaml/dune/commit/28545f3f729d6b203bc3f4f5bd5bda5ba975bb3a"
        },
        "date": 1677187640362,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.54175735352",
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
          "id": "609ff7e9e4a1efd80da139e8990315587538e547",
          "message": "test: reproducible 7146 (#7174)\n\nAnother attempt to make this test work everywhere\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-23T18:21:26-08:00",
          "tree_id": "270bfe3633272189f39ce8d515e95612710c63a3",
          "url": "https://github.com/ocaml/dune/commit/609ff7e9e4a1efd80da139e8990315587538e547"
        },
        "date": 1677206560722,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "38.11308922849333",
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
          "id": "dc9510a30d2b47a31d25af2047bc3ec51089cabc",
          "message": "doc: add a lexer for dune files (#7079)\n\nThis replaces the uses of \"scheme\" and \"lisp\" lexers.\r\n\r\nThe lexer is fairly non-opinionated and tries to stick to just the\r\nlexing of dune-lang (atoms, strings, things like pforms, etc), and does\r\nnot have a list of known stanzas and field names for example.\r\n\r\nIt does two special things:\r\n\r\n- it recognizes metasyntax like `<arg>` and highlights it accordingly to\r\n  show that these parts are not meant to be put verbatim in dune files\r\n- it highlights the first atom in lists in a different way. This\r\n  corresponds to how most of dune-lang works, but is not totally\r\n  correct.\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>",
          "timestamp": "2023-02-24T14:36:57+01:00",
          "tree_id": "48d5ebbc560b5f046ddcd03c5607bd0a8b8d3444",
          "url": "https://github.com/ocaml/dune/commit/dc9510a30d2b47a31d25af2047bc3ec51089cabc"
        },
        "date": 1677246944767,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "36.025471917653334",
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
          "id": "8de60a99e3d73810417f6026bc9788f5732436c2",
          "message": "test: move install tests (#7161)\n\nMove install tests to install/\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-24T08:41:16-08:00",
          "tree_id": "1b52cd9bdb532dd61562132b414485e7f9d7de7e",
          "url": "https://github.com/ocaml/dune/commit/8de60a99e3d73810417f6026bc9788f5732436c2"
        },
        "date": 1677258050895,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "37.874068813633336",
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
          "id": "927b672beb3cc1c39aa54701b51cccf88ab3bc8c",
          "message": "refactor: use Filename.t more consistently (#7172)\n\nUse it in the signatures of [Source_tree] and [Path]\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-24T09:04:52-08:00",
          "tree_id": "e5dbc707f615c0df99136426ef2d1e7cc110195d",
          "url": "https://github.com/ocaml/dune/commit/927b672beb3cc1c39aa54701b51cccf88ab3bc8c"
        },
        "date": 1677259750419,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "45.056681189513334",
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
          "id": "6fa1a17bebc0114ee0b1febf3d215be3c028351c",
          "message": "fix: Revert \"fix: Invoke preprocessor commands from local dir\" (#7169)\n\nRunning the preprocessor from cwd breaks the error messages outputted by\r\nthe preprocessor.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-24T13:02:09-08:00",
          "tree_id": "db56da9f0a9bf2bb38516707f38ca29f95811644",
          "url": "https://github.com/ocaml/dune/commit/6fa1a17bebc0114ee0b1febf3d215be3c028351c"
        },
        "date": 1677273635204,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.29539175910667",
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
          "id": "89d73f2918269925fc548f3e36a0de23f0636e49",
          "message": "refactor(path): remove dead code (#7180)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-26T09:35:13-08:00",
          "tree_id": "9cd9208b6419a55a3049bdb46f90a24da2383e3f",
          "url": "https://github.com/ocaml/dune/commit/89d73f2918269925fc548f3e36a0de23f0636e49"
        },
        "date": 1677434339910,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "43.961991473926666",
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
          "id": "0c4504db4df28895737ab5093c63ffad6fd2e3d4",
          "message": "fix: do not re-render unless needed (#7186)\n\nPreviously, dune would re-render on every frame even when it wasn't\r\nnecessary.\r\n\r\nNow, dune will make sure we have at least one modification before\r\nre-rendering\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-26T14:42:26-08:00",
          "tree_id": "625ed44ff5bce6a069c9d709d59f36a272838429",
          "url": "https://github.com/ocaml/dune/commit/0c4504db4df28895737ab5093c63ffad6fd2e3d4"
        },
        "date": 1677452504183,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.11455215426",
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
          "id": "43c27fb6accccf1589d0d04ea964bc87cdd70c9d",
          "message": "test: add pkg config to ctypes test (#7121)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-26T14:54:31-08:00",
          "tree_id": "10ed4bf350da816bc5dca0507e81a0dc3d1b28d6",
          "url": "https://github.com/ocaml/dune/commit/43c27fb6accccf1589d0d04ea964bc87cdd70c9d"
        },
        "date": 1677453200280,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "32.9659399369",
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
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "distinct": true,
          "id": "5c967b158fe7b43e4386a9bc1c54a450dcfd7417",
          "message": "fix: always start from lib_subdir when installing module sources\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-02-27T12:04:02-08:00",
          "tree_id": "38875b9ee117b500601fe0a889fbd21d20b50ac2",
          "url": "https://github.com/ocaml/dune/commit/5c967b158fe7b43e4386a9bc1c54a450dcfd7417"
        },
        "date": 1677529403633,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "36.285840466926665",
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
          "id": "ea424da26ffbbf255a6f64998bd798d0ebaa3e2a",
          "message": "feature: concurrency action (#6933)\n\n* feature: concurrency action\r\n\r\nWe add a (concurrent ) action which acts like (progn ) the difference\r\nbeing the actions contained within can be executed concurrently by Dune.\r\n\r\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-02-27T13:40:43-08:00",
          "tree_id": "2af52312315dbb8441b7338e11787dd83cb2f991",
          "url": "https://github.com/ocaml/dune/commit/ea424da26ffbbf255a6f64998bd798d0ebaa3e2a"
        },
        "date": 1677535171977,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.92991475566",
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
          "id": "ebc21f0a9556f910ab12dc179bb0c12bbea5ae13",
          "message": "fix(install): respect display options (#7116)\n\n* fix(install): respect display options\r\n\r\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-02-27T17:37:40-08:00",
          "tree_id": "20ebc7f0f64b16489a2ee6c3a9018b614ac14666",
          "url": "https://github.com/ocaml/dune/commit/ebc21f0a9556f910ab12dc179bb0c12bbea5ae13"
        },
        "date": 1677549406468,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.80079535251334",
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
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "distinct": true,
          "id": "6324a4a35cbe156f14b06b78c8a1c63b42c8a82e",
          "message": "test(stdlib): merge wrapped/unwrapped tests\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-27T17:53:36-08:00",
          "tree_id": "b56cb321714760a40764579b6815fe54ae701619",
          "url": "https://github.com/ocaml/dune/commit/6324a4a35cbe156f14b06b78c8a1c63b42c8a82e"
        },
        "date": 1677550327408,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.878910772926666",
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
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "distinct": true,
          "id": "e58ff4dea9073ad331cf172fcc16f3d4710af0f7",
          "message": "test: install libraries in the same source\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-27T19:53:49-06:00",
          "tree_id": "ddf68609a00107f6511e376500632fbb45c0ac82",
          "url": "https://github.com/ocaml/dune/commit/e58ff4dea9073ad331cf172fcc16f3d4710af0f7"
        },
        "date": 1677561754104,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.922827373893334",
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
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "distinct": true,
          "id": "fc0397efc5f0b9208b2d6d93b287d5cb3ddcb554",
          "message": "fix: revert accidental push\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-28T10:49:33-06:00",
          "tree_id": "b56cb321714760a40764579b6815fe54ae701619",
          "url": "https://github.com/ocaml/dune/commit/fc0397efc5f0b9208b2d6d93b287d5cb3ddcb554"
        },
        "date": 1677604142030,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "36.33735649658001",
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
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "distinct": true,
          "id": "6324a4a35cbe156f14b06b78c8a1c63b42c8a82e",
          "message": "test(stdlib): merge wrapped/unwrapped tests\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-27T17:53:36-08:00",
          "tree_id": "b56cb321714760a40764579b6815fe54ae701619",
          "url": "https://github.com/ocaml/dune/commit/6324a4a35cbe156f14b06b78c8a1c63b42c8a82e"
        },
        "date": 1677604618697,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "43.29519833493999",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "5031221+voodoos@users.noreply.github.com",
            "name": "Ulysse",
            "username": "voodoos"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "93e8a95088e8437e7ddd64a5b1fc9a3500fef6d2",
          "message": "Add test illustrating #6575 (#6576)\n\nSigned-off-by: Ulysse Gérard <thevoodoos@gmail.com>",
          "timestamp": "2023-02-28T10:38:13-08:00",
          "tree_id": "379ff33814986639bfcc27214fe40f646cb33253",
          "url": "https://github.com/ocaml/dune/commit/93e8a95088e8437e7ddd64a5b1fc9a3500fef6d2"
        },
        "date": 1677610615141,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.89898680954",
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
          "id": "bf5697dccb058ca2a38f46e604debd8084d4d932",
          "message": "chore: add rule streaming proposal (#7195)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-28T10:37:50-08:00",
          "tree_id": "f829cb46169117ba00ec458aea929236439f42e2",
          "url": "https://github.com/ocaml/dune/commit/bf5697dccb058ca2a38f46e604debd8084d4d932"
        },
        "date": 1677610639572,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "37.44799662044667",
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
          "id": "3f75e20b3c920c9662d328b9af2cd1ff7611046b",
          "message": "Test case for bug report (#6725)\n\nTest the situation where we have:\r\n\r\n1. an empty library A\r\n2. an executable with a module A\r\n3. conditional module selection in the exe\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-28T11:02:53-08:00",
          "tree_id": "0e5b1bdd5c4bbae52a7dd079dd470f671c6a2741",
          "url": "https://github.com/ocaml/dune/commit/3f75e20b3c920c9662d328b9af2cd1ff7611046b"
        },
        "date": 1677612180320,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "39.32016590114667",
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
          "id": "f36109e8924c5d47cf06b632a86d858734bc1e4d",
          "message": "perf: add synthetic benchmark (#7189)\n\n* perf: add synthetic benchmark\r\n\r\nSigned-off-by: Javier Chávarri <javier.chavarri@gmail.com>",
          "timestamp": "2023-02-28T11:52:20-08:00",
          "tree_id": "6c9300c3a5b760644e0282b619c9480ee9e83c27",
          "url": "https://github.com/ocaml/dune/commit/f36109e8924c5d47cf06b632a86d858734bc1e4d"
        },
        "date": 1677615061488,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.326087940693334",
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
          "id": "497c0e33271dafa8a1440d24faed7b99872dc548",
          "message": "melange: interpret `melc --where` as a list of `:`-separated paths (#7176)\n\n* melange: interpret `melc --where` as a list of `:`-separated paths\r\n\r\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-02-28T12:28:43-08:00",
          "tree_id": "c8dc3fd4b2a98ae41ba6f8d8cd6d5c3dd2d719cf",
          "url": "https://github.com/ocaml/dune/commit/497c0e33271dafa8a1440d24faed7b99872dc548"
        },
        "date": 1677617317650,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "38.7594802454",
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
          "id": "4e83d1b80bd9a37de7e0d39d6f22873850314557",
          "message": "test: duplicate packages in vendor dir (#7194)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-28T12:31:15-08:00",
          "tree_id": "2f033d89248006aaab12e19ac5ed99ee1ca95cfc",
          "url": "https://github.com/ocaml/dune/commit/4e83d1b80bd9a37de7e0d39d6f22873850314557"
        },
        "date": 1677617462621,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "38.04882729859333",
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
          "id": "5de6e9f0946727f3cab329f9442273c0bfcca3cf",
          "message": "test(melange): add a test that introduces rules in the target dir (#7196)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-02-28T14:48:51-08:00",
          "tree_id": "14f8003f16339e644cede9e74c31d6e51564d12d",
          "url": "https://github.com/ocaml/dune/commit/5de6e9f0946727f3cab329f9442273c0bfcca3cf"
        },
        "date": 1677625704445,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "36.80109491081999",
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
          "id": "b21df142505fc40b9760fa23b6d039ea8585b15e",
          "message": "test(melange): add test exercising #7104 (#7204)\n\n* test(melange): add test exercising #7104\r\n\r\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-01T11:21:41-08:00",
          "tree_id": "c38ec00bad886535b35d1dbc4e2db82392fa3eaf",
          "url": "https://github.com/ocaml/dune/commit/b21df142505fc40b9760fa23b6d039ea8585b15e"
        },
        "date": 1677700034618,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.93125169972",
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
          "id": "b8fd9dffecfd1a102720ca2ab485e80fcee2cb65",
          "message": "fix: custom log file path (#7200)\n\nCreate directory when using a custom path for the log file\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-01T11:24:30-08:00",
          "tree_id": "4713ed049c7d250ddd6841136f703fb08bcadd45",
          "url": "https://github.com/ocaml/dune/commit/b8fd9dffecfd1a102720ca2ab485e80fcee2cb65"
        },
        "date": 1677700160919,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.268939123606664",
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
          "id": "f6deaf9dd20458276b61df9eeb23381cc9846ab7",
          "message": "test: use sh in concurrent test (#7205)\n\nno need to switch to bash\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-01T15:10:41-08:00",
          "tree_id": "c44bbeb5ef287241bb3d1a4761f983dc063860b2",
          "url": "https://github.com/ocaml/dune/commit/f6deaf9dd20458276b61df9eeb23381cc9846ab7"
        },
        "date": 1677713396335,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.63444994974",
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
          "id": "e909c5a3ad09944a52854ce2f2c5e4d57e86ff9b",
          "message": "test: vendored and public libs (#7197)\n\nDemonstrate that a public library is currently allowed to depend on a\r\npublic library that is vendored. This should not work.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-01T17:23:38-08:00",
          "tree_id": "8c70311d7c6bdb0940aaca7f65a78fedc5506333",
          "url": "https://github.com/ocaml/dune/commit/e909c5a3ad09944a52854ce2f2c5e4d57e86ff9b"
        },
        "date": 1677721627060,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "41.64735710306666",
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
          "id": "013a0205d831242a97c699ec731e0e060baf1521",
          "message": "benchmark: add warm run (#7198)\n\nSigned-off-by: Javier Chávarri <javier.chavarri@gmail.com>",
          "timestamp": "2023-03-02T08:14:27-08:00",
          "tree_id": "c13569935c6591a70022a618f6ae51b858320ff9",
          "url": "https://github.com/ocaml/dune/commit/013a0205d831242a97c699ec731e0e060baf1521"
        },
        "date": 1677774800176,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.378816458073324",
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
          "id": "53f06748757eb600dd6c81705b52fdead50af727",
          "message": "fix(rules): don't descend into automatic subdirs infinitely (#7208)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-02T11:13:11-08:00",
          "tree_id": "a7950772a83a60cfa187ac773a18a1bfde836f41",
          "url": "https://github.com/ocaml/dune/commit/53f06748757eb600dd6c81705b52fdead50af727"
        },
        "date": 1677785541440,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "36.332690064513336",
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
          "id": "a75e28cd778e05bcde8239f5df926ac349b2d172",
          "message": "doc(coq): update documentation about coqdep\n\n<!-- ps-id: 429b0fd3-7098-410b-96a4-80d1b39c4072 -->\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-02T22:24:50+01:00",
          "tree_id": "5f2ece398a4ac6ff73d27ce329711dae8f9845a8",
          "url": "https://github.com/ocaml/dune/commit/a75e28cd778e05bcde8239f5df926ac349b2d172"
        },
        "date": 1677793695761,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "42.468556154939996",
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
          "id": "8d88ee8068abb053fe8ed7c9c21b3a1883dbaf47",
          "message": "feature: add terminal ui backend based on NoTTY (#6996)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-02T13:48:18-08:00",
          "tree_id": "fed526d9dc61ac26685496aeb92a7b07d214d084",
          "url": "https://github.com/ocaml/dune/commit/8d88ee8068abb053fe8ed7c9c21b3a1883dbaf47"
        },
        "date": 1677794941961,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.17965460165333",
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
          "id": "5bdb8444d202d55aeb71e68bd8b26a9f0b4358ea",
          "message": "perf: run parse_compilation_units once (#7187)\n\nfix: speed up compilation with many modules\r\n\r\nOnly generate the map used to look up dependencies once per library/executable. Rather than once per module.\r\n\r\nSigned-off-by: Javier Chávarri <javier.chavarri@gmail.com>\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>\r\nCo-authored-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-03T14:59:30-08:00",
          "tree_id": "8e1a40d009380c9ba654d90156bd4ee3f4eb639f",
          "url": "https://github.com/ocaml/dune/commit/5bdb8444d202d55aeb71e68bd8b26a9f0b4358ea"
        },
        "date": 1677885880516,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "36.707560031073335",
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
          "id": "ce02bf035c7c421167be3b7b706f2b934faa6f59",
          "message": "feature(ansi_color): add support for 8 and 24 bit color codes (#7188)\n\nAnsi escape codes for 8-bit and 24-bit colors will no longer be dropped.\r\n\r\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-03T15:08:29-08:00",
          "tree_id": "318e336a11b12824df8dcf9cbf53feb6fc3d84ef",
          "url": "https://github.com/ocaml/dune/commit/ce02bf035c7c421167be3b7b706f2b934faa6f59"
        },
        "date": 1677886641149,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "40.49125791864",
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
          "id": "40a5eae5bf6871db10c55983dee3506d3e4b8dad",
          "message": "test: disable concurrent.t on macos (#7212)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-03T15:09:52-08:00",
          "tree_id": "6e1b1064053ed570b20f1a485cc8ad2ae7c9c3e5",
          "url": "https://github.com/ocaml/dune/commit/40a5eae5bf6871db10c55983dee3506d3e4b8dad"
        },
        "date": 1677886844298,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "43.838432390659996",
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
          "id": "ee6846e9e4e42f3ae2bca8accabc705c139facab",
          "message": "fix(melange): typo in error message (#7213)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-03T15:51:27-08:00",
          "tree_id": "646aae28e249551deabef20c26b9e75f9bccdf25",
          "url": "https://github.com/ocaml/dune/commit/ee6846e9e4e42f3ae2bca8accabc705c139facab"
        },
        "date": 1677888904807,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "41.72650621152666",
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
          "id": "436965f550c40fd19fbe29d8a4b19f9b75bf8914",
          "message": "test: re-enable actions test on macos (#7215)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-03T18:42:23-08:00",
          "tree_id": "60be2b993aa5adad561a6a4764ab152bb30fd71d",
          "url": "https://github.com/ocaml/dune/commit/436965f550c40fd19fbe29d8a4b19f9b75bf8914"
        },
        "date": 1677898870306,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.39084838723333",
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
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "distinct": true,
          "id": "a658bd6c754a9ad2a55c01ea92fc0e3dea942557",
          "message": "fix: disallow nested melange.emit stanzas\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-05T20:01:06-08:00",
          "tree_id": "94aa0c568f7726f771a66960b236c637e218bafe",
          "url": "https://github.com/ocaml/dune/commit/a658bd6c754a9ad2a55c01ea92fc0e3dea942557"
        },
        "date": 1678077005261,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.45420881360666",
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
          "id": "77489f7cd89ee362fa7475da91aaca1b400ddce0",
          "message": "test: remove unnecessary shell script (#7209)\n\ninstead of sdune, just export an environment variable to enable\r\nsandboxing\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-07T07:04:03-08:00",
          "tree_id": "9cb2bbecc04d3ce663ce9b598884de0ab98f3173",
          "url": "https://github.com/ocaml/dune/commit/77489f7cd89ee362fa7475da91aaca1b400ddce0"
        },
        "date": 1678202584277,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.26893459143333",
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
          "id": "c28016854046cd2bdbf521939d5eae4a9eadc96c",
          "message": "feat(melange): add `runtime_deps` to copy assets to the target dir (#7199)\n\n* feat(melange): add `runtime_deps` to copy assets to the target dir\r\n\r\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-07T08:00:57-08:00",
          "tree_id": "88b6219604d85faf5ed2eb7c16edf6446b29bc88",
          "url": "https://github.com/ocaml/dune/commit/c28016854046cd2bdbf521939d5eae4a9eadc96c"
        },
        "date": 1678206347301,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "44.25444881012",
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
          "id": "ac0f50314c89f9da4e53310505faf4e21e6f9fc6",
          "message": "feature: use clonefile on macos (#7210)\n\nUse mac's [clonefile] instead of manually copying. [clonefile] is like\r\nhardlink but it will copy-on-write when edited.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-07T08:38:03-08:00",
          "tree_id": "05d8958bf2f214d3bd6b05e15788815ed0418e95",
          "url": "https://github.com/ocaml/dune/commit/ac0f50314c89f9da4e53310505faf4e21e6f9fc6"
        },
        "date": 1678208210819,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "32.793703105493336",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "ivg@ieee.org",
            "name": "Ivan Gotovchits",
            "username": "ivg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "086a78cc56b25a4da5795671698d7ef4d1473a21",
          "message": "adds support for loading plugins in toplevels (#6082)\n\n* adds support for loading plugins in toplevels\r\n\r\nUses virtual libraries to select the proper dynamic linker\r\nfacility. To load in the toplevel add `dune-site.toplevel` library as\r\nthe dependency to your toplevel.\r\n\r\n3. Also, handle moved load_file function.  Prior to OCaml 4.13.0, the\r\nload_file function was in Topdirs.  Starting with OCaml 4.13.0, the\r\nload_file function moved to Toploop. In order to find it open both\r\nthese modules, suppressing the warning for unused open, and then\r\nreference load_file unqualified.\r\n\r\nSigned-off-by: Richard L Ford <richardlford@gmail.com>\r\nSigned-off-by: ivg <ivg@ieee.org>\r\nCo-authored-by: Richard L Ford <richardlford@gmail.com>",
          "timestamp": "2023-03-07T10:56:52-08:00",
          "tree_id": "4d0b1d0bf8c7d9c57a95f271146f9dc6c992e502",
          "url": "https://github.com/ocaml/dune/commit/086a78cc56b25a4da5795671698d7ef4d1473a21"
        },
        "date": 1678216738778,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "39.720539269026666",
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
          "id": "a0dd51512f269d7c18f0c6216ab66d3ab7368da2",
          "message": "chore: add clonefile on macos optimization to CHANGES (#7237)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-07T11:45:52-08:00",
          "tree_id": "b471c4175c60ce05265ef19667bad2d9238c68fc",
          "url": "https://github.com/ocaml/dune/commit/a0dd51512f269d7c18f0c6216ab66d3ab7368da2"
        },
        "date": 1678219493096,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.07062802632",
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
          "id": "a3ff6bf48bb5e203bfda1afaf1fe0b69ae77103d",
          "message": "Show that multiple licenses are supported (#7098)\n\nThe stanza is in the singular, but in fact supports more than one\r\nlicense. A lot of `.opam.template` files exist that add a `license`\r\nfield to be able to specify multiple licenses where this is not strictly\r\nrequired.\r\n\r\nSigned-off-by: Marek Kubica <marek@tarides.com>",
          "timestamp": "2023-03-08T14:46:55Z",
          "tree_id": "05a7593b486388c1fb85169ce2d7e9d232e0f076",
          "url": "https://github.com/ocaml/dune/commit/a3ff6bf48bb5e203bfda1afaf1fe0b69ae77103d"
        },
        "date": 1678287969911,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.60195301587334",
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
          "id": "16b7df37807be538ba33adc5b7d55888c589205e",
          "message": "test: fix macOS test suite (#7236)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-08T11:51:03-08:00",
          "tree_id": "07ac0d15890807f368fde05aa4c7d71ccfefa501",
          "url": "https://github.com/ocaml/dune/commit/16b7df37807be538ba33adc5b7d55888c589205e"
        },
        "date": 1678306221532,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.829971563133334",
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
          "id": "3b934509f88b996d5118fe394e378514544d1494",
          "message": "test: sigpipe handling (#7242)\n\nWe show that sigpipe currently kills dune\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-08T12:04:18-08:00",
          "tree_id": "e5fe1bc41931c1d1984c5f3effdef92a8166c7d1",
          "url": "https://github.com/ocaml/dune/commit/3b934509f88b996d5118fe394e378514544d1494"
        },
        "date": 1678307224636,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "41.412890504900005",
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
          "id": "ebf90a3fdefc16604a97df3a627d7c33b68a48c4",
          "message": "coq_config: add dyn and improve comments\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>\n\n<!-- ps-id: 9bb3f6fb-281a-47e3-b7fb-138d85becd81 -->",
          "timestamp": "2023-03-08T21:22:00+01:00",
          "tree_id": "cafadc3013ac8cae4c0fb5240bf65c0013213217",
          "url": "https://github.com/ocaml/dune/commit/ebf90a3fdefc16604a97df3a627d7c33b68a48c4"
        },
        "date": 1678308109770,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.84141596917334",
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
          "id": "f60f7216dcfd3bae9d37ddfdca042278bb9a5229",
          "message": "loc: make pp_file_colon_line polymorphic (#7243)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-08T12:18:37-08:00",
          "tree_id": "5d6ba820ccaa5920e72f2a7ede236bf58f5522f8",
          "url": "https://github.com/ocaml/dune/commit/f60f7216dcfd3bae9d37ddfdca042278bb9a5229"
        },
        "date": 1678308138848,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "38.779683553859996",
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
          "id": "3802efe8100508bb60eb599e0a105f4556c887d5",
          "message": "coq_config: Add coqcorelib\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>\n\n<!-- ps-id: 1d5e63eb-4cb5-466f-8270-bf05bd1fe056 -->",
          "timestamp": "2023-03-08T21:42:14+01:00",
          "tree_id": "431953a65bb9985cb86ff5e44ea820fd4dd83293",
          "url": "https://github.com/ocaml/dune/commit/3802efe8100508bb60eb599e0a105f4556c887d5"
        },
        "date": 1678309494047,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "41.292520837839994",
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
          "id": "357348710e9c00dd5a8cea6074b9187e8e2a5e77",
          "message": "ci(coq): bump coq version to 8.16.1\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>\n\n<!-- ps-id: 90edb35a-a48c-4b1a-99f5-ef48b01352ae -->",
          "timestamp": "2023-03-08T22:06:41+01:00",
          "tree_id": "8084c0cf8cc1a44895ec8891955d8ea384b650b1",
          "url": "https://github.com/ocaml/dune/commit/357348710e9c00dd5a8cea6074b9187e8e2a5e77"
        },
        "date": 1678311092574,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "45.29232647927333",
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
          "id": "634bc0ebbf9f85dddb849c065a2d9caf4e7a3071",
          "message": "test(coq): duplicate theory in a project\n\nAppears to work correctly.\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>\n\n<!-- ps-id: 45dfb6a8-bff8-48ae-8ad7-fa0ca6b41d12 -->",
          "timestamp": "2023-03-08T23:20:00+01:00",
          "tree_id": "91a11ed7cde64337ffc350ef7fe43244fefefcf0",
          "url": "https://github.com/ocaml/dune/commit/634bc0ebbf9f85dddb849c065a2d9caf4e7a3071"
        },
        "date": 1678315637349,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "41.599692099786665",
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
          "id": "44c01c1a413b700d4297efa83fc56c3e78b2c03d",
          "message": "refactor: use `Pp.enumerate` in 2 places (#7252)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-08T15:29:58-08:00",
          "tree_id": "4992d73585e05be46f12e0c49e654bb246f2a338",
          "url": "https://github.com/ocaml/dune/commit/44c01c1a413b700d4297efa83fc56c3e78b2c03d"
        },
        "date": 1678319320040,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "32.57960347976667",
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
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "distinct": true,
          "id": "c7a0049e24e6d802a46f7ed34abf42bc525bf89d",
          "message": "fix(melange): two emit stanzas in a directory\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-08T15:31:10-08:00",
          "tree_id": "10156b26142855e6044ee31b2f2131bae98be703",
          "url": "https://github.com/ocaml/dune/commit/c7a0049e24e6d802a46f7ed34abf42bc525bf89d"
        },
        "date": 1678319622714,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "41.49915973624666",
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
          "id": "d429530c59f13ffdee023bc78ee607ac9850ce96",
          "message": "feat(melange): support `(select ...)` in `melange.emit` (#7239)\n\n* feat(melange): support `(select ...)` in `melange.emit`\r\n\r\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-08T17:06:25-08:00",
          "tree_id": "f5e91d65b8b2b70979230785ddcf7ef8d3bd8d14",
          "url": "https://github.com/ocaml/dune/commit/d429530c59f13ffdee023bc78ee607ac9850ce96"
        },
        "date": 1678325189602,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "37.70884585486667",
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
          "id": "3e99fc556abbedee00a95a0e38bddf5ab8980ac8",
          "message": "test(memo): demonstrate loss of concurrency (#7251)\n\nReproduces the loss of concurrency observed in #5549 in a unit test\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-09T03:35:08Z",
          "tree_id": "37f08e73bd140fe5885838d3c04a62db51c35199",
          "url": "https://github.com/ocaml/dune/commit/3e99fc556abbedee00a95a0e38bddf5ab8980ac8"
        },
        "date": 1678334053213,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.560735836646664",
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
          "id": "c82312b020bcee6f047ab588691ae02d0f0a5bf7",
          "message": "chore: fix melange in the main default devShell (#7256)\n\n* chore: fix melange in the main default devShell\r\n* fix: simplify flake, get dune version from the melange overlay\r\n\r\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-09T13:50:47-08:00",
          "tree_id": "d3407acc89f49bb9e0c4c8d7d9b318d27a1a146f",
          "url": "https://github.com/ocaml/dune/commit/c82312b020bcee6f047ab588691ae02d0f0a5bf7"
        },
        "date": 1678400548224,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "45.70110720228001",
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
          "id": "d5582c7d677b0d8afcc92f5c390d014cdac1a97f",
          "message": "chore(nix): remove the opam2nix dependency on the default package (#7258)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-09T15:09:35-08:00",
          "tree_id": "0e579760f62fdb226997e861522d4c545a9d5816",
          "url": "https://github.com/ocaml/dune/commit/d5582c7d677b0d8afcc92f5c390d014cdac1a97f"
        },
        "date": 1678404527518,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.83634540134",
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
          "id": "8e1567dd0ed31ad8fadb32df87e36cc65a0928e8",
          "message": "coq: improve directory target message (#7250)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-10T08:42:30-08:00",
          "tree_id": "97da383ba2b1bffa21d44ff73989a7de6e9d8bb0",
          "url": "https://github.com/ocaml/dune/commit/8e1567dd0ed31ad8fadb32df87e36cc65a0928e8"
        },
        "date": 1678468394315,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.660425479353336",
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
          "id": "e52c89bc2939982426b2f3dc27d1e04631f76d01",
          "message": "test(coq): duplicate theory in a dune file\n\nAppears to be bugged since the directory targets are being reported\nfirst.\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>\n\n<!-- ps-id: 028d7bfb-988a-44e2-ae57-72fcfeee8d5f -->",
          "timestamp": "2023-03-10T17:51:32+01:00",
          "tree_id": "6ea29e142e086ce099e456b41e048e639b9678dd",
          "url": "https://github.com/ocaml/dune/commit/e52c89bc2939982426b2f3dc27d1e04631f76d01"
        },
        "date": 1678468748482,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.188954251919995",
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
          "id": "eec2b08d68cfe8408e662b8e5e897d2c0b8d366d",
          "message": "fix(coq): fix duplicate dir targets being detected before theories (#7263)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-10T09:40:51-08:00",
          "tree_id": "286b637bce1c9d866d72a50b6822baa53ea1b01c",
          "url": "https://github.com/ocaml/dune/commit/eec2b08d68cfe8408e662b8e5e897d2c0b8d366d"
        },
        "date": 1678471279657,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "38.05452444876",
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
          "id": "4c696272ad42bbb3f3f6603f52571b7cd92b8e3e",
          "message": "chore(makefile): update OCaml to 4.14.1 (#7265)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-10T12:17:35-08:00",
          "tree_id": "710714f31ae25e9ff9baa69d5e08558073d838af",
          "url": "https://github.com/ocaml/dune/commit/4c696272ad42bbb3f3f6603f52571b7cd92b8e3e"
        },
        "date": 1678480838789,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "42.210326727853335",
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
          "id": "82df8daf476d615681061b9d513710d2cb16daf6",
          "message": "chore(docker): add perf to image (#7266)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-10T12:44:09-08:00",
          "tree_id": "583fd28cd16e279964412f91f9e7b91fda8c4437",
          "url": "https://github.com/ocaml/dune/commit/82df8daf476d615681061b9d513710d2cb16daf6"
        },
        "date": 1678483802345,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "42.19475211186",
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
          "id": "0cff92b585fd5edeca88a6c70d2134e2266bd89e",
          "message": "chore(docker): preserve history between sessions (#7269)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-10T16:27:54-08:00",
          "tree_id": "5760bcf178d2d2aeba545c34d07fcf952ab251d6",
          "url": "https://github.com/ocaml/dune/commit/0cff92b585fd5edeca88a6c70d2134e2266bd89e"
        },
        "date": 1678495630699,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.86812630236",
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
          "id": "1402ef79b486ec740adecd1572c8e421146cb9c4",
          "message": "fix(boot): fix env vars when finding out concurrency (#7272)\n\nWe were passing an empty enviornment to Unix.open_process_full_cmd\r\nbefore. We now pass the full available Unix.environment since we are\r\nonly querying the concurrency by calling nproc or getconf.\r\n\r\nOn NixOS this was causing the bootstrap to default to a single\r\nprocessor since nproc wasn't available in the empty environment.\r\n\r\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-10T16:45:05-08:00",
          "tree_id": "627252095722073f3c929ba8841c945b7f494bb7",
          "url": "https://github.com/ocaml/dune/commit/1402ef79b486ec740adecd1572c8e421146cb9c4"
        },
        "date": 1678496615824,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "32.67541120256001",
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
          "id": "57c8e78875d6ca90bc050bc5c6ebf52094f20953",
          "message": "feature(boot): remove reliance on shell (#7274)\n\nPreviously, we'd run the shell to discover the number of processes by\r\nusing `Unix.open_process_full`. The shell isn't being used for anything,\r\nso we switch to executing the process directly.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-10T18:39:26-08:00",
          "tree_id": "9a47839dbcec4fe5e2c7a7ec5b33e7b6576cde21",
          "url": "https://github.com/ocaml/dune/commit/57c8e78875d6ca90bc050bc5c6ebf52094f20953"
        },
        "date": 1678503495361,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "32.840060836940005",
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
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "distinct": true,
          "id": "1260dc53c1b3fcdb1cee8c8c6d9c2209a5e59428",
          "message": "fix(Io.copy_file): fail fast if src is a directory\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-10T21:56:56-08:00",
          "tree_id": "167f0a33811644e36b1303ef53e498f3e04a596f",
          "url": "https://github.com/ocaml/dune/commit/1260dc53c1b3fcdb1cee8c8c6d9c2209a5e59428"
        },
        "date": 1678515357586,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "32.68902143498",
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
          "id": "a3bbd92ef265423de4c05371ea20576b974935e1",
          "message": "refactor(stdune.io): share directory check (#7278)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-10T22:09:41-08:00",
          "tree_id": "be8f37723ee34ec640bcfb1ee15842d9178f33b4",
          "url": "https://github.com/ocaml/dune/commit/a3bbd92ef265423de4c05371ea20576b974935e1"
        },
        "date": 1678516398249,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "41.630771744700006",
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
          "id": "50387d3bc8f095ae4ea0d681de775a46580c5899",
          "message": "chore(nix): share test deps in all shells (#7275)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-10T22:23:21-08:00",
          "tree_id": "2a6feeeb5777fbbddf5eadebb396436341d363c6",
          "url": "https://github.com/ocaml/dune/commit/50387d3bc8f095ae4ea0d681de775a46580c5899"
        },
        "date": 1678516956624,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.99283658254001",
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
          "id": "848a5e764f108dc31a595343eb18fc76462761d9",
          "message": "test: reproducibility of sigpipe.t (#7273)\n\nCreate the output file before running dune so that the final `cat` never\r\nfails.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>\r\n\r\n<!-- ps-id: f95e7a20-622a-4e24-be18-25f119d68ef8 -->",
          "timestamp": "2023-03-11T09:03:19-08:00",
          "tree_id": "972841937b2710114e0de4b1547740159aa98bc7",
          "url": "https://github.com/ocaml/dune/commit/848a5e764f108dc31a595343eb18fc76462761d9"
        },
        "date": 1678555347664,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.544430228",
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
          "id": "71a950fc2d7a01c75d1745ce364f2aa370a7ba47",
          "message": "doc: cleanup support seciton of readme and remove jbuilder mentions (#7283)\n\nIt has been quite a while since the jbuilder migration so I think we\r\ncan safely remove any mention of it in the README.md.\r\n\r\nI've also improved the support section pointing to GitHub discussions\r\nand issues. I am not sure the ocaml-core email is being checked by\r\nanybody so I have removed it.\r\n\r\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-11T09:08:16-08:00",
          "tree_id": "1b9f720fe9a374fa94b5ffe2d1db49a80fc33ecf",
          "url": "https://github.com/ocaml/dune/commit/71a950fc2d7a01c75d1745ce364f2aa370a7ba47"
        },
        "date": 1678555632170,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.10160527210667",
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
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "00cf75ae1612c9f0fd8f9d57c32446da3f12d38d",
          "message": "chore: pin but not install melange\n\nSo that we install melange along with the rest of the deps at once\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>\n\n<!-- ps-id: 88ca4ba3-3e2a-4b53-b303-0cb2b596faba -->",
          "timestamp": "2023-03-11T20:25:51+01:00",
          "tree_id": "83dedfacae91c8a45e23901c35f6477eb321ae88",
          "url": "https://github.com/ocaml/dune/commit/00cf75ae1612c9f0fd8f9d57c32446da3f12d38d"
        },
        "date": 1678563852248,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "32.830353887293334",
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
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "b3a59e514b290f7135ceb572377f6f0d700e38d0",
          "message": "fix(console): clean up console on exit\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>\n\n<!-- ps-id: 12e55e26-139f-4c14-a1c8-62a97001fbed -->",
          "timestamp": "2023-03-11T20:30:11+01:00",
          "tree_id": "25947e4e5a327fc348182d2199c16ff2f81c7e98",
          "url": "https://github.com/ocaml/dune/commit/b3a59e514b290f7135ceb572377f6f0d700e38d0"
        },
        "date": 1678564129206,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.00351708439333",
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
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "67d714edfab99366e32ff3a809a8456c7ff078f3",
          "message": "fix(nix): fix duplicate packages warning\n\nafter https://github.com/NixOS/nixpkgs/pull/205646, Nix detects\nconflicting findlib libraries in the `$OCAMLPATH`. Because we use 2\nnixpkgs sources, patdiff was running into that case\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-11T22:15:28+01:00",
          "tree_id": "4728bb430367e557addd09b8083ccbf77f5babc5",
          "url": "https://github.com/ocaml/dune/commit/67d714edfab99366e32ff3a809a8456c7ff078f3"
        },
        "date": 1678570441904,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.14034234458",
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
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "095d4a87e5e9d9acb176070fcb120ba1c8435bc7",
          "message": "fix(nix): conflicting ocamlfind dependencies\n\nIn #7258, we removed opam2nix from the `default` package, but the\ndefault shell needs to depend on the correct one (now dune rather than\ndefault)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-11T23:53:00+01:00",
          "tree_id": "2ef3707ca5f8b36d9a7a727e79f3a80ca46f6af0",
          "url": "https://github.com/ocaml/dune/commit/095d4a87e5e9d9acb176070fcb120ba1c8435bc7"
        },
        "date": 1678576312207,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "37.341341048539995",
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
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "7a6974cf1b9faa055f22d72c7b39f3df13212337",
          "message": "chore: docs, consolidate rule streaming and production\n\nSigned-off-by: Javier Chávarri <javier.chavarri@gmail.com>",
          "timestamp": "2023-03-12T01:01:35+01:00",
          "tree_id": "48a94b5bfa9f6c778955e0f7fe0a5f25567faf63",
          "url": "https://github.com/ocaml/dune/commit/7a6974cf1b9faa055f22d72c7b39f3df13212337"
        },
        "date": 1678580482276,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "36.57932737441333",
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
          "id": "4a0efff885e4216ffca7aa3cecf7418a656fc230",
          "message": "nix: remove coq from the main flake\n\nCoq has its own flake anyway.\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>\n\n<!-- ps-id: 3663fd3e-0ef5-4686-8ab4-77c28706648f -->",
          "timestamp": "2023-03-12T01:03:27+01:00",
          "tree_id": "04e1c771bae133130f1fee98722f5841d013ce8f",
          "url": "https://github.com/ocaml/dune/commit/4a0efff885e4216ffca7aa3cecf7418a656fc230"
        },
        "date": 1678580756435,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "42.11972453725333",
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
          "id": "c5cadcf535fe099aa5cdc323f064a97cbd47e4c6",
          "message": "nix: add ocamlformat to testNativeBuildInputs in flake\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>\n\n<!-- ps-id: 1b96f5c7-fea3-4927-a48b-1ccf8f009e8d -->",
          "timestamp": "2023-03-12T01:25:02+01:00",
          "tree_id": "4427ca414e329a846423b5c2df9318e0a966572b",
          "url": "https://github.com/ocaml/dune/commit/c5cadcf535fe099aa5cdc323f064a97cbd47e4c6"
        },
        "date": 1678581875529,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "36.91428856325333",
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
          "id": "54e6ce1ebaac4b5854d60bf0e17874d777cd4fbf",
          "message": "nix: add formatter\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>\n\n<!-- ps-id: ec5d9ef8-d1fc-443b-93fb-c9670bda26c9 -->",
          "timestamp": "2023-03-12T01:24:43+01:00",
          "tree_id": "6f7701da24eb6a251918beed04c050518b226a70",
          "url": "https://github.com/ocaml/dune/commit/54e6ce1ebaac4b5854d60bf0e17874d777cd4fbf"
        },
        "date": 1678581884773,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.665486777506665",
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
          "id": "f9a0e6f00bfc731df60f44a7544eecc7dfaa7778",
          "message": "test(stdune): more copying tests (#7284)\n\ncover more error cases:\r\n- missing src\r\n- src is a directory\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-11T16:30:24-08:00",
          "tree_id": "faa0dc353332556f05e605cd937e0aec1af2655e",
          "url": "https://github.com/ocaml/dune/commit/f9a0e6f00bfc731df60f44a7544eecc7dfaa7778"
        },
        "date": 1678582136019,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.66373989152667",
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
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "64cf42dc58da7a6be862fc28e7721db7d9652788",
          "message": "Apply suggestions from code review\n\nCo-authored-by: Ali Caglayan <alizter@gmail.com>\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-12T01:56:22+01:00",
          "tree_id": "0327a7034a981c7453944c8bfac315ffec7ab19a",
          "url": "https://github.com/ocaml/dune/commit/64cf42dc58da7a6be862fc28e7721db7d9652788"
        },
        "date": 1678583708543,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.35587353498667",
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
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "73ead70ca2e3d0a029f8adae0817502b1b0dae5e",
          "message": "fix(stdune): copy file error message on macos\n\nRemoe the additional \"error:\" prefix\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>\n\n<!-- ps-id: 759a4dea-1244-4591-ad38-cecd44adea5b -->",
          "timestamp": "2023-03-12T01:59:15+01:00",
          "tree_id": "51cb81c219c70fef6cf17a503c9f053622276838",
          "url": "https://github.com/ocaml/dune/commit/73ead70ca2e3d0a029f8adae0817502b1b0dae5e"
        },
        "date": 1678583855833,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.67195140998667",
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
          "id": "449cbd322ca2a2c50d437d2a2b69d81d5bdf0439",
          "message": "chore: improve file copying benchmark (#7294)\n\nallow changing the directory and the size of files\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-11T22:13:19-08:00",
          "tree_id": "5ce869c6ce3e4c036e9b8c727748a3bf3276d1d9",
          "url": "https://github.com/ocaml/dune/commit/449cbd322ca2a2c50d437d2a2b69d81d5bdf0439"
        },
        "date": 1678602759208,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "36.31958073630666",
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
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "789264ff111a79e3a2d760bf1de55a286c4a0396",
          "message": "fix(nix): use `extraBuildInputs` in `mkSlim`\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-12T22:00:17+01:00",
          "tree_id": "350b3364a6cb94f78a52f4c78264a6a5eadb52f7",
          "url": "https://github.com/ocaml/dune/commit/789264ff111a79e3a2d760bf1de55a286c4a0396"
        },
        "date": 1678655913239,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.255844440733334",
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
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "e5a98ee12081cf3571e5a6d4d1aa14ddf392eb0d",
          "message": "fix(stdune): copy file error message consistency\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>\n\n<!-- ps-id: 97fe8b6e-0238-496c-b1d2-0683b775adb5 -->",
          "timestamp": "2023-03-12T22:11:15+01:00",
          "tree_id": "a3d2a4f33bcd0f6c19ea09b866740081575d7819",
          "url": "https://github.com/ocaml/dune/commit/e5a98ee12081cf3571e5a6d4d1aa14ddf392eb0d"
        },
        "date": 1678656801679,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "41.715459426020004",
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
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "0c0d37eca9d47c0d19620a976cd57971188abc0c",
          "message": "Add repro for #7034\n\nSigned-off-by: Stephen Sherratt <stephen@sherra.tt>\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-13T01:13:03+01:00",
          "tree_id": "afc456c550342e2887e6da0c21b086f9f3148484",
          "url": "https://github.com/ocaml/dune/commit/0c0d37eca9d47c0d19620a976cd57971188abc0c"
        },
        "date": 1678667491613,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.65215277272",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "amokhov@janestreet.com",
            "name": "Andrey Mokhov",
            "username": "snowleopard"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "35e3958215e0fed860aecb24e977a46448950103",
          "message": "Add commutative monoids (#7240)\n\nSigned-off-by: Andrey Mokhov <andrey.mokhov@gmail.com>",
          "timestamp": "2023-03-13T15:13:27+08:00",
          "tree_id": "ab05c6b45656466be6764df730b089744059bc34",
          "url": "https://github.com/ocaml/dune/commit/35e3958215e0fed860aecb24e977a46448950103"
        },
        "date": 1678692755046,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "37.64377397316",
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
          "id": "d7c0f37f8f64a5b3449aa09eae7c09b9ea2f5ea1",
          "message": "fix(melange): copy, don't symlink, runtime_deps (#7267)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-13T10:13:09-07:00",
          "tree_id": "54740154af335555581ca25f2017e0d4689f4a97",
          "url": "https://github.com/ocaml/dune/commit/d7c0f37f8f64a5b3449aa09eae7c09b9ea2f5ea1"
        },
        "date": 1678728766670,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "38.25631099415333",
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
          "id": "988634c6c4a6a24c81fca523c59ff17a7467e193",
          "message": "fix(melange): rename `entries` to `modules` in `melange.emit` (#7302)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-13T10:17:41-07:00",
          "tree_id": "b388e03afc4f5bd16df2ef4cd544b05f5f7703fc",
          "url": "https://github.com/ocaml/dune/commit/988634c6c4a6a24c81fca523c59ff17a7467e193"
        },
        "date": 1678729057663,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "37.11993693375334",
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
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "b444ea9f36511286a7ddb9c3e9ffef16fb03536b",
          "message": "chore(nix): remove OCaml nix-overlays\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-13T20:01:46+01:00",
          "tree_id": "790071e21212acb32eefc52f8ef86c9aabedd0e9",
          "url": "https://github.com/ocaml/dune/commit/b444ea9f36511286a7ddb9c3e9ffef16fb03536b"
        },
        "date": 1678735210790,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.794725980066666",
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
          "id": "21914b91f66a94e2cae33b9b19ea1521b6104d8a",
          "message": "fix(rpc): ignore sigpipe on macos clients (#7299)\n\nRPC clients that suddenly disconnect will no longer raise sigpipe and\r\nmake dune terminate.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-13T12:26:10-07:00",
          "tree_id": "8912e6faccd7d85a04c9ad3400af1070e1cda50f",
          "url": "https://github.com/ocaml/dune/commit/21914b91f66a94e2cae33b9b19ea1521b6104d8a"
        },
        "date": 1678736747114,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.94071393932666",
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
          "id": "66523e542511028a6cb72d80471f358aab30d2eb",
          "message": "feat(melange): add `melange.runtime_deps` field in library (#7234)\n\n* feat(melange): add `melange.runtime_deps` field in library\r\n\r\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-13T16:10:16-07:00",
          "tree_id": "c3c02da57a85c69afcc5e465890bb9d86f4c6127",
          "url": "https://github.com/ocaml/dune/commit/66523e542511028a6cb72d80471f358aab30d2eb"
        },
        "date": 1678750729819,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.80919181778666",
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
          "id": "7e27809863060fe315bfeebef262990b46e89e87",
          "message": "refactor: rename pthread_chdir_stubs (#7310)\n\nit no longer just contains stubs for chdir\r\n\r\nalso take the opportunity to format it automatically\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-13T16:10:51-07:00",
          "tree_id": "02ff87c75f5d23539847e101abd284af5c3b3012",
          "url": "https://github.com/ocaml/dune/commit/7e27809863060fe315bfeebef262990b46e89e87"
        },
        "date": 1678751212798,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "43.47160125782",
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
          "id": "8621946251ef39e52c115cb36d79d5f56818dd04",
          "message": "fix(stdlib): pass flags when building stdlib.ml (#7241)\n\nReported by @gretay-js.\r\n\r\nThis ensures that when building `stdlib.ml` (the main module of a\r\nlibrary with `(stdlib)`), flags set in the corresponding stanza\r\n`(library)` are correctly passed.\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>",
          "timestamp": "2023-03-14T10:08:46+01:00",
          "tree_id": "f455d9414c9c8ae0c0e6767b07525663d9ca935d",
          "url": "https://github.com/ocaml/dune/commit/8621946251ef39e52c115cb36d79d5f56818dd04"
        },
        "date": 1678786043328,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "36.37674712466001",
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
          "id": "9a36c025e228115963bc462d21ec948dff24b7a6",
          "message": "test: do not load init toplevel files (#7315)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-14T10:23:51-07:00",
          "tree_id": "4f004d014e22c9ec462a98ce5ec1ca2142d7c065",
          "url": "https://github.com/ocaml/dune/commit/9a36c025e228115963bc462d21ec948dff24b7a6"
        },
        "date": 1678815947131,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "39.630143694666664",
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
          "id": "6374c6fe43b044771b26ad461efbe4ba975a6842",
          "message": "refactor(rpc): hide Session.create (#7317)\n\nIt was never used outside of the Csexp_rpc module\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-14T12:59:28-07:00",
          "tree_id": "68a92befe23eca28d4215cdb67abd21bfb67b639",
          "url": "https://github.com/ocaml/dune/commit/6374c6fe43b044771b26ad461efbe4ba975a6842"
        },
        "date": 1678825065847,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.47911317024667",
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
          "id": "00fdccc5bdc423e5437c5ece0cff0316d31669aa",
          "message": "test: move toplevel plugins to own dir (#7316)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-14T13:00:46-07:00",
          "tree_id": "9d473a3d041916135fc9f531b67a0dde2b8245ca",
          "url": "https://github.com/ocaml/dune/commit/00fdccc5bdc423e5437c5ece0cff0316d31669aa"
        },
        "date": 1678825163309,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "35.67868703714",
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
          "id": "59cc644a039107e97d6d45dd8a046a26d15b8e22",
          "message": "test: odoc improvements (#7305)\n\nmove odoc test to odoc/ dir\r\nmove odoc test stanzas to odoc/ dir\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-14T13:35:26-07:00",
          "tree_id": "b444805a44904ecf08beb07a7163fdaf89a5ddc9",
          "url": "https://github.com/ocaml/dune/commit/59cc644a039107e97d6d45dd8a046a26d15b8e22"
        },
        "date": 1678827265394,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.123133418773335",
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
          "id": "aafa99289d7f570f4a998aaa88364f06c073180f",
          "message": "refactor(csexp_rpc): remove useless socket arg (#7318)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>\r\n\r\n<!-- ps-id: ec04f08e-5c43-4325-af38-6b32ae471217 -->",
          "timestamp": "2023-03-14T13:37:55-07:00",
          "tree_id": "8ee35a42bf1da96c17f0a1458ce3fd08fac9d3b9",
          "url": "https://github.com/ocaml/dune/commit/aafa99289d7f570f4a998aaa88364f06c073180f"
        },
        "date": 1678827417799,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.877083432473334",
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
          "id": "846831c7cc2443db5f3aa0e12a43d9ba48cdd639",
          "message": "fix(ci): restrict jsoo version (#7324)\n\nOur test suite is incompatible with 5.1.0.\r\nThis ensures it is not picked by CI.\r\nThis bound is to be removed once a fixed version has been released.\r\n\r\nSee ocsigen/js_of_ocaml#1430\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>",
          "timestamp": "2023-03-15T16:24:37+01:00",
          "tree_id": "e7c95f691bdf8c30aeb622b813faff546141dd7d",
          "url": "https://github.com/ocaml/dune/commit/846831c7cc2443db5f3aa0e12a43d9ba48cdd639"
        },
        "date": 1678895186140,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "41.380115095979995",
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
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "distinct": true,
          "id": "ef9bc1df2213541bdd91170516a8031d9766c7ac",
          "message": "fix(melange): allow runtime_deps in the melange target dir\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-15T19:38:35-07:00",
          "tree_id": "cc1f1f039cd2dccac16444cd4593d192af40e3d9",
          "url": "https://github.com/ocaml/dune/commit/ef9bc1df2213541bdd91170516a8031d9766c7ac"
        },
        "date": 1678935760424,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "32.675908281919995",
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
          "id": "745dca81e0adc6deab9e4bfaed33e985c80641dd",
          "message": "chore(nix): upgrade melange that picks up `entries` -> `modules` rename (#7320)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-15T19:47:47-07:00",
          "tree_id": "75416581c02cf21f49986b5f66c8b2eb4cd451c7",
          "url": "https://github.com/ocaml/dune/commit/745dca81e0adc6deab9e4bfaed33e985c80641dd"
        },
        "date": 1678936321395,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "36.459350920086656",
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
          "id": "be1187e1306370a2d77cd8f79ce6c4e2baa13a7d",
          "message": "refactor(melange): propagate runtime_deps `loc` in Melange_rules (#7331)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-15T20:35:23-07:00",
          "tree_id": "7b15271f2ae8ac3437f0363f3d04762ef68395bc",
          "url": "https://github.com/ocaml/dune/commit/be1187e1306370a2d77cd8f79ce6c4e2baa13a7d"
        },
        "date": 1678939163467,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "46.041377149413336",
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
          "id": "20ad5e109e3879a54e988fd4f9b2922c7a6d5fb9",
          "message": "fix(rules): merge allowed subdirs correctly for Redirect_to_parent rules (#7207)\n\n* fix(rules): merge allowed subdirs correctly for Redirect_to_parent rules",
          "timestamp": "2023-03-16T00:18:54-07:00",
          "tree_id": "99e07b91df680686e63c2b9e76e077def14e25b3",
          "url": "https://github.com/ocaml/dune/commit/20ad5e109e3879a54e988fd4f9b2922c7a6d5fb9"
        },
        "date": 1678952452264,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "40.47026432668667",
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
          "id": "a29a12950f8d8f97ecc84fe3f70d28144111075b",
          "message": "test(melange): switch to sh (#7332)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-16T00:19:23-07:00",
          "tree_id": "e19141c8b1c3b4f7a234eac38f283c47787ec7c0",
          "url": "https://github.com/ocaml/dune/commit/a29a12950f8d8f97ecc84fe3f70d28144111075b"
        },
        "date": 1678952535453,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "41.75675455794",
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
          "id": "c0c0acba6b4d0744198e0ce06dae0095ff0c6384",
          "message": "Add monorepo benchmark (#7202)\n\n* Add monorepo benchmark\r\n\r\nThis adds a current-bench benchmark that measures the time it takes to\r\nbuild a large monorepo composed from opam packages.\r\n\r\nSigned-off-by: Stephen Sherratt <stephen@sherra.tt>",
          "timestamp": "2023-03-16T08:30:40-07:00",
          "tree_id": "8c5741ec18ba2fe104c2774da6863ad1b8a68e0c",
          "url": "https://github.com/ocaml/dune/commit/c0c0acba6b4d0744198e0ce06dae0095ff0c6384"
        },
        "date": 1678981858599,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.64421996734",
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
          "id": "649031728346482a83441c26c0bbe06ecd89cce2",
          "message": "Update opam-file-format to 2.1.5 (#7328)\n\n* Update opam-file-format to 2.1.5\r\n\r\nSigned-off-by: Marek Kubica <marek@tarides.com>",
          "timestamp": "2023-03-16T09:22:38-07:00",
          "tree_id": "a4abe01551c0de278b22657cfa623b0897ec74ec",
          "url": "https://github.com/ocaml/dune/commit/649031728346482a83441c26c0bbe06ecd89cce2"
        },
        "date": 1678984883424,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.02812130647333",
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
          "id": "22eab0558fd06cdb0c3ba7ae8cf0074fb7c17a55",
          "message": "refactor: rename dune_config (#7333)\n\nRename dune_config to dune_config_file. We can still access the old\r\nnames by opening Dune_config.\r\n\r\nIn preparation for introducing a dune_config library that will introduce\r\na [Dune_config.Config] module.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-16T19:25:43-07:00",
          "tree_id": "c56d6df09a65cf0d012a8e35a3d13848e14b3a3a",
          "url": "https://github.com/ocaml/dune/commit/22eab0558fd06cdb0c3ba7ae8cf0074fb7c17a55"
        },
        "date": 1679021275519,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "39.897304929946664",
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
          "id": "6ac9daba4d18b7fe52a2026881279dc3a6680583",
          "message": "benchmarks: add synthetic watch (#7255)\n\n* benchmarks: add synthetic watch\r\n\r\nSigned-off-by: Javier Chávarri <javier.chavarri@gmail.com>",
          "timestamp": "2023-03-16T19:28:14-07:00",
          "tree_id": "9f89b7728daca543771684882ade802631ee6751",
          "url": "https://github.com/ocaml/dune/commit/6ac9daba4d18b7fe52a2026881279dc3a6680583"
        },
        "date": 1679021421230,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "36.03218512904001",
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
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "557e5089782b2441a5b4dd9198d4ebff11299d85",
          "message": "raise error when using describe pp command with staged_pps (#7167)\n\nSigned-off-by: Corentin Leruth <corentin.leruth@gmail.com>",
          "timestamp": "2023-03-16T19:41:51-07:00",
          "tree_id": "47f95224d1846aa25a8f83db8e2139d5fbb7ab89",
          "url": "https://github.com/ocaml/dune/commit/557e5089782b2441a5b4dd9198d4ebff11299d85"
        },
        "date": 1679022257825,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "36.135718577253336",
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
          "id": "695a4506a6f0a72023a509f8ef78734b17b5f23a",
          "message": "Revert \"fix(ci): restrict jsoo version\" (#7342)\n\nThis reverts commit 846831c7cc2443db5f3aa0e12a43d9ba48cdd639.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-17T08:51:25-07:00",
          "tree_id": "ae7ea767488f2005175054106784e24b04ecf11f",
          "url": "https://github.com/ocaml/dune/commit/695a4506a6f0a72023a509f8ef78734b17b5f23a"
        },
        "date": 1679069635297,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.67899490122667",
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
          "id": "8cdeba0ed881fdec2b369d584012d4f95242e323",
          "message": "fix(rpc): ignore sigpipe on linux (#7319)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-17T09:09:07-07:00",
          "tree_id": "9c21adffaadd084b20dc2d3d6965317f55dc9ceb",
          "url": "https://github.com/ocaml/dune/commit/8cdeba0ed881fdec2b369d584012d4f95242e323"
        },
        "date": 1679070681332,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "32.25237646512",
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
          "id": "72d8172b408f97cc6efebd28eb348c2fa0dee879",
          "message": "refactor(engine): remove source tree traversals (#7150)\n\nPreviously, the engine would know about directories for computing\r\nsource_tree, and recursive aliases.\r\n\r\nWith this PR, these operations are generalized to work over arbitrary\r\ndirectory traversals.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-17T09:39:49-07:00",
          "tree_id": "63631fddb3edcc00aa9c8e89e7f07927bb942e82",
          "url": "https://github.com/ocaml/dune/commit/72d8172b408f97cc6efebd28eb348c2fa0dee879"
        },
        "date": 1679072872609,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "44.61135456728667",
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
          "id": "dfd5d29fffe5a1e85688ffb3941022983eae7525",
          "message": "Update doc/hacking.rst\n\nCo-authored-by: Andrey Mokhov <andrey.mokhov@gmail.com>\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-17T18:23:18+01:00",
          "tree_id": "9054e5f6d0a99a3a53726a54dc1dec1988e29e03",
          "url": "https://github.com/ocaml/dune/commit/dfd5d29fffe5a1e85688ffb3941022983eae7525"
        },
        "date": 1679075286031,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "38.433224213773336",
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
          "id": "8b031d58633e5c9bac0a9fbced9fa27d719bb305",
          "message": "test(coq): add coq_scrub_args.sh script\n\nAdded a script `coq_scrub_args.sh` which will scrub the output of\ncommon command printing. This is useful for checking that the correct\nflags are being passed and saves us from having to mess around with\nsed everytime we wish to test something like this.\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-17T18:19:28+01:00",
          "tree_id": "d3c08dfc46410c191e138121bdb1d4fc0d875e8a",
          "url": "https://github.com/ocaml/dune/commit/8b031d58633e5c9bac0a9fbced9fa27d719bb305"
        },
        "date": 1679075339596,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "43.49111681028666",
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
          "id": "8bea7ea00018f58112b3b8a68faa73cb87552412",
          "message": "feature: add a thread pool (#7201)\n\nreplace the current implementation with an unbounded number of workers\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-17T10:46:10-07:00",
          "tree_id": "cf2d1d281f98f277bc144c69678fd2e17e070c70",
          "url": "https://github.com/ocaml/dune/commit/8bea7ea00018f58112b3b8a68faa73cb87552412"
        },
        "date": 1679076494098,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.328358889499995",
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
          "id": "4245029f2914bcfdca915e9916a5155a0afa95fb",
          "message": "feature: add action runners (#6737)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-17T16:47:22-07:00",
          "tree_id": "c854afadcc9812714d9415eca4c789aeb33a416e",
          "url": "https://github.com/ocaml/dune/commit/4245029f2914bcfdca915e9916a5155a0afa95fb"
        },
        "date": 1679098156660,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "36.00519235772667",
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
          "id": "c4fc52b3f1e68620d3a9712d46d4f710f0c09fb7",
          "message": "Doc: fix default location of cache on Windows (#7356)\n\nSigned-off-by: Nicolás Ojeda Bär <n.oje.bar@gmail.com>",
          "timestamp": "2023-03-20T15:06:35+01:00",
          "tree_id": "54b9c616aa715efd1b625d36735db5ac4280079d",
          "url": "https://github.com/ocaml/dune/commit/c4fc52b3f1e68620d3a9712d46d4f710f0c09fb7"
        },
        "date": 1679322514182,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.27740159946",
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
          "id": "cc174110c2dcab83b0068825870e1484cecef464",
          "message": "Doc: split \"General concepts\" into several pages (#7232)\n\nAs discussed with @Alizter, this splits our giant \"General concepts\" page into\r\nsmaller ones.\r\n\r\nIn addition, this transforms references to the various sections into :doc:\r\nreferences to the documents themselves.\r\n\r\nThe content is mostly untouched, except:\r\n\r\n\"see the X section\" -> \"see X\" (now that it's not a section anymore)\r\nthere was a mixup between the \"sandboxing\" and \"dune action plugin\" references\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>\r\nCo-authored-by: Christine Rose <christinerose@users.noreply.github.com>",
          "timestamp": "2023-03-20T17:21:42+01:00",
          "tree_id": "872414ea39310a1aa2495271c0ed34998d5d9223",
          "url": "https://github.com/ocaml/dune/commit/cc174110c2dcab83b0068825870e1484cecef464"
        },
        "date": 1679330651969,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "34.89964033055333",
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
          "id": "247664d098d6fe8e464f46169f9bf30006581d7b",
          "message": "Run inline tests in parallel (#7012)\n\n* feature(inline-tests): partitions\r\n\r\nAllow specifying partitions of inline tests. Partitions in the same test\r\nsuite will be executed concurrently.\r\n\r\nSigned-off-by: Hugo Heuzard <hugo.heuzard@gmail.com>",
          "timestamp": "2023-03-20T12:24:58-07:00",
          "tree_id": "6c4134e0c5f5f1efc83fdc6008ad079f40e633c0",
          "url": "https://github.com/ocaml/dune/commit/247664d098d6fe8e464f46169f9bf30006581d7b"
        },
        "date": 1679341604520,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "33.66236321636",
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
          "id": "b1e0bbb3ce44f3df569d7669d946b735939b39c0",
          "message": "dune coq top: fix capitialization in error messages\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-21T01:00:39+01:00",
          "tree_id": "047518031c07995f0ae2263bea79dcb3c4fe38e5",
          "url": "https://github.com/ocaml/dune/commit/b1e0bbb3ce44f3df569d7669d946b735939b39c0"
        },
        "date": 1679358522887,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "36.13605722782666",
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
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "distinct": true,
          "id": "d920ff4e9bb0cab87b50ba968383ecc6a50e6c95",
          "message": "test(rpc): add test with server in eager mode\n\nSigned-off-by: Javier Chávarri <javier.chavarri@gmail.com>",
          "timestamp": "2023-03-20T17:36:34-07:00",
          "tree_id": "d8228291c6dbbf85af549247bff950e8343f7fd7",
          "url": "https://github.com/ocaml/dune/commit/d920ff4e9bb0cab87b50ba968383ecc6a50e6c95"
        },
        "date": 1679360270699,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "pupilfirst build time (Linux)",
            "value": "32.54813430094",
            "unit": "seconds"
          }
        ]
      }
    ],
    "Synthetic Benchmark": [
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
          "id": "f36109e8924c5d47cf06b632a86d858734bc1e4d",
          "message": "perf: add synthetic benchmark (#7189)\n\n* perf: add synthetic benchmark\r\n\r\nSigned-off-by: Javier Chávarri <javier.chavarri@gmail.com>",
          "timestamp": "2023-02-28T11:52:20-08:00",
          "tree_id": "6c9300c3a5b760644e0282b619c9480ee9e83c27",
          "url": "https://github.com/ocaml/dune/commit/f36109e8924c5d47cf06b632a86d858734bc1e4d"
        },
        "date": 1677615424303,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "71.4238815637",
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
          "id": "497c0e33271dafa8a1440d24faed7b99872dc548",
          "message": "melange: interpret `melc --where` as a list of `:`-separated paths (#7176)\n\n* melange: interpret `melc --where` as a list of `:`-separated paths\r\n\r\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-02-28T12:28:43-08:00",
          "tree_id": "c8dc3fd4b2a98ae41ba6f8d8cd6d5c3dd2d719cf",
          "url": "https://github.com/ocaml/dune/commit/497c0e33271dafa8a1440d24faed7b99872dc548"
        },
        "date": 1677617707670,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "77.18277223948667",
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
          "id": "4e83d1b80bd9a37de7e0d39d6f22873850314557",
          "message": "test: duplicate packages in vendor dir (#7194)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-02-28T12:31:15-08:00",
          "tree_id": "2f033d89248006aaab12e19ac5ed99ee1ca95cfc",
          "url": "https://github.com/ocaml/dune/commit/4e83d1b80bd9a37de7e0d39d6f22873850314557"
        },
        "date": 1677617845954,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "75.68696638700668",
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
          "id": "5de6e9f0946727f3cab329f9442273c0bfcca3cf",
          "message": "test(melange): add a test that introduces rules in the target dir (#7196)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-02-28T14:48:51-08:00",
          "tree_id": "14f8003f16339e644cede9e74c31d6e51564d12d",
          "url": "https://github.com/ocaml/dune/commit/5de6e9f0946727f3cab329f9442273c0bfcca3cf"
        },
        "date": 1677626082986,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "75.21734505147333",
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
          "id": "b21df142505fc40b9760fa23b6d039ea8585b15e",
          "message": "test(melange): add test exercising #7104 (#7204)\n\n* test(melange): add test exercising #7104\r\n\r\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-01T11:21:41-08:00",
          "tree_id": "c38ec00bad886535b35d1dbc4e2db82392fa3eaf",
          "url": "https://github.com/ocaml/dune/commit/b21df142505fc40b9760fa23b6d039ea8585b15e"
        },
        "date": 1677700393260,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "70.74899626214666",
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
          "id": "b8fd9dffecfd1a102720ca2ab485e80fcee2cb65",
          "message": "fix: custom log file path (#7200)\n\nCreate directory when using a custom path for the log file\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-01T11:24:30-08:00",
          "tree_id": "4713ed049c7d250ddd6841136f703fb08bcadd45",
          "url": "https://github.com/ocaml/dune/commit/b8fd9dffecfd1a102720ca2ab485e80fcee2cb65"
        },
        "date": 1677700522981,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "71.63756332902666",
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
          "id": "f6deaf9dd20458276b61df9eeb23381cc9846ab7",
          "message": "test: use sh in concurrent test (#7205)\n\nno need to switch to bash\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-01T15:10:41-08:00",
          "tree_id": "c44bbeb5ef287241bb3d1a4761f983dc063860b2",
          "url": "https://github.com/ocaml/dune/commit/f6deaf9dd20458276b61df9eeb23381cc9846ab7"
        },
        "date": 1677713769060,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "73.44171568489332",
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
          "id": "e909c5a3ad09944a52854ce2f2c5e4d57e86ff9b",
          "message": "test: vendored and public libs (#7197)\n\nDemonstrate that a public library is currently allowed to depend on a\r\npublic library that is vendored. This should not work.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-01T17:23:38-08:00",
          "tree_id": "8c70311d7c6bdb0940aaca7f65a78fedc5506333",
          "url": "https://github.com/ocaml/dune/commit/e909c5a3ad09944a52854ce2f2c5e4d57e86ff9b"
        },
        "date": 1677722055555,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "84.33481449831334",
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
          "id": "013a0205d831242a97c699ec731e0e060baf1521",
          "message": "benchmark: add warm run (#7198)\n\nSigned-off-by: Javier Chávarri <javier.chavarri@gmail.com>",
          "timestamp": "2023-03-02T08:14:27-08:00",
          "tree_id": "c13569935c6591a70022a618f6ae51b858320ff9",
          "url": "https://github.com/ocaml/dune/commit/013a0205d831242a97c699ec731e0e060baf1521"
        },
        "date": 1677775146580,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "67.25241026476",
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
          "id": "013a0205d831242a97c699ec731e0e060baf1521",
          "message": "benchmark: add warm run (#7198)\n\nSigned-off-by: Javier Chávarri <javier.chavarri@gmail.com>",
          "timestamp": "2023-03-02T08:14:27-08:00",
          "tree_id": "c13569935c6591a70022a618f6ae51b858320ff9",
          "url": "https://github.com/ocaml/dune/commit/013a0205d831242a97c699ec731e0e060baf1521"
        },
        "date": 1677775173080,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "4.715359799073334",
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
          "id": "53f06748757eb600dd6c81705b52fdead50af727",
          "message": "fix(rules): don't descend into automatic subdirs infinitely (#7208)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-02T11:13:11-08:00",
          "tree_id": "a7950772a83a60cfa187ac773a18a1bfde836f41",
          "url": "https://github.com/ocaml/dune/commit/53f06748757eb600dd6c81705b52fdead50af727"
        },
        "date": 1677785911709,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "73.05451115488667",
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
          "id": "53f06748757eb600dd6c81705b52fdead50af727",
          "message": "fix(rules): don't descend into automatic subdirs infinitely (#7208)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-02T11:13:11-08:00",
          "tree_id": "a7950772a83a60cfa187ac773a18a1bfde836f41",
          "url": "https://github.com/ocaml/dune/commit/53f06748757eb600dd6c81705b52fdead50af727"
        },
        "date": 1677785940069,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "5.1826786787",
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
          "id": "a75e28cd778e05bcde8239f5df926ac349b2d172",
          "message": "doc(coq): update documentation about coqdep\n\n<!-- ps-id: 429b0fd3-7098-410b-96a4-80d1b39c4072 -->\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-02T22:24:50+01:00",
          "tree_id": "5f2ece398a4ac6ff73d27ce329711dae8f9845a8",
          "url": "https://github.com/ocaml/dune/commit/a75e28cd778e05bcde8239f5df926ac349b2d172"
        },
        "date": 1677794136297,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "86.89284885161332",
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
          "id": "a75e28cd778e05bcde8239f5df926ac349b2d172",
          "message": "doc(coq): update documentation about coqdep\n\n<!-- ps-id: 429b0fd3-7098-410b-96a4-80d1b39c4072 -->\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-02T22:24:50+01:00",
          "tree_id": "5f2ece398a4ac6ff73d27ce329711dae8f9845a8",
          "url": "https://github.com/ocaml/dune/commit/a75e28cd778e05bcde8239f5df926ac349b2d172"
        },
        "date": 1677794169387,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "6.025551723646667",
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
          "id": "8d88ee8068abb053fe8ed7c9c21b3a1883dbaf47",
          "message": "feature: add terminal ui backend based on NoTTY (#6996)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-02T13:48:18-08:00",
          "tree_id": "fed526d9dc61ac26685496aeb92a7b07d214d084",
          "url": "https://github.com/ocaml/dune/commit/8d88ee8068abb053fe8ed7c9c21b3a1883dbaf47"
        },
        "date": 1677795284780,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "67.51444762060001",
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
          "id": "8d88ee8068abb053fe8ed7c9c21b3a1883dbaf47",
          "message": "feature: add terminal ui backend based on NoTTY (#6996)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-02T13:48:18-08:00",
          "tree_id": "fed526d9dc61ac26685496aeb92a7b07d214d084",
          "url": "https://github.com/ocaml/dune/commit/8d88ee8068abb053fe8ed7c9c21b3a1883dbaf47"
        },
        "date": 1677795311094,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "4.744268833626666",
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
          "id": "5bdb8444d202d55aeb71e68bd8b26a9f0b4358ea",
          "message": "perf: run parse_compilation_units once (#7187)\n\nfix: speed up compilation with many modules\r\n\r\nOnly generate the map used to look up dependencies once per library/executable. Rather than once per module.\r\n\r\nSigned-off-by: Javier Chávarri <javier.chavarri@gmail.com>\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>\r\nCo-authored-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-03T14:59:30-08:00",
          "tree_id": "8e1a40d009380c9ba654d90156bd4ee3f4eb639f",
          "url": "https://github.com/ocaml/dune/commit/5bdb8444d202d55aeb71e68bd8b26a9f0b4358ea"
        },
        "date": 1677886237670,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "70.26600356962666",
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
          "id": "5bdb8444d202d55aeb71e68bd8b26a9f0b4358ea",
          "message": "perf: run parse_compilation_units once (#7187)\n\nfix: speed up compilation with many modules\r\n\r\nOnly generate the map used to look up dependencies once per library/executable. Rather than once per module.\r\n\r\nSigned-off-by: Javier Chávarri <javier.chavarri@gmail.com>\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>\r\nCo-authored-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-03T14:59:30-08:00",
          "tree_id": "8e1a40d009380c9ba654d90156bd4ee3f4eb639f",
          "url": "https://github.com/ocaml/dune/commit/5bdb8444d202d55aeb71e68bd8b26a9f0b4358ea"
        },
        "date": 1677886249061,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.8785241936733337",
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
          "id": "ce02bf035c7c421167be3b7b706f2b934faa6f59",
          "message": "feature(ansi_color): add support for 8 and 24 bit color codes (#7188)\n\nAnsi escape codes for 8-bit and 24-bit colors will no longer be dropped.\r\n\r\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-03T15:08:29-08:00",
          "tree_id": "318e336a11b12824df8dcf9cbf53feb6fc3d84ef",
          "url": "https://github.com/ocaml/dune/commit/ce02bf035c7c421167be3b7b706f2b934faa6f59"
        },
        "date": 1677887041415,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "78.95683498375332",
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
          "id": "ce02bf035c7c421167be3b7b706f2b934faa6f59",
          "message": "feature(ansi_color): add support for 8 and 24 bit color codes (#7188)\n\nAnsi escape codes for 8-bit and 24-bit colors will no longer be dropped.\r\n\r\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-03T15:08:29-08:00",
          "tree_id": "318e336a11b12824df8dcf9cbf53feb6fc3d84ef",
          "url": "https://github.com/ocaml/dune/commit/ce02bf035c7c421167be3b7b706f2b934faa6f59"
        },
        "date": 1677887055033,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "2.0985649265466666",
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
          "id": "40a5eae5bf6871db10c55983dee3506d3e4b8dad",
          "message": "test: disable concurrent.t on macos (#7212)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-03T15:09:52-08:00",
          "tree_id": "6e1b1064053ed570b20f1a485cc8ad2ae7c9c3e5",
          "url": "https://github.com/ocaml/dune/commit/40a5eae5bf6871db10c55983dee3506d3e4b8dad"
        },
        "date": 1677887258663,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "80.68347123810666",
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
          "id": "40a5eae5bf6871db10c55983dee3506d3e4b8dad",
          "message": "test: disable concurrent.t on macos (#7212)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-03T15:09:52-08:00",
          "tree_id": "6e1b1064053ed570b20f1a485cc8ad2ae7c9c3e5",
          "url": "https://github.com/ocaml/dune/commit/40a5eae5bf6871db10c55983dee3506d3e4b8dad"
        },
        "date": 1677887272625,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "2.18793707132",
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
          "id": "ee6846e9e4e42f3ae2bca8accabc705c139facab",
          "message": "fix(melange): typo in error message (#7213)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-03T15:51:27-08:00",
          "tree_id": "646aae28e249551deabef20c26b9e75f9bccdf25",
          "url": "https://github.com/ocaml/dune/commit/ee6846e9e4e42f3ae2bca8accabc705c139facab"
        },
        "date": 1677889326275,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "84.409041852",
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
          "id": "ee6846e9e4e42f3ae2bca8accabc705c139facab",
          "message": "fix(melange): typo in error message (#7213)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-03T15:51:27-08:00",
          "tree_id": "646aae28e249551deabef20c26b9e75f9bccdf25",
          "url": "https://github.com/ocaml/dune/commit/ee6846e9e4e42f3ae2bca8accabc705c139facab"
        },
        "date": 1677889340553,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "2.2622258283666667",
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
          "id": "436965f550c40fd19fbe29d8a4b19f9b75bf8914",
          "message": "test: re-enable actions test on macos (#7215)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-03T18:42:23-08:00",
          "tree_id": "60be2b993aa5adad561a6a4764ab152bb30fd71d",
          "url": "https://github.com/ocaml/dune/commit/436965f550c40fd19fbe29d8a4b19f9b75bf8914"
        },
        "date": 1677899223000,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "69.65386287670667",
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
          "id": "436965f550c40fd19fbe29d8a4b19f9b75bf8914",
          "message": "test: re-enable actions test on macos (#7215)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-03T18:42:23-08:00",
          "tree_id": "60be2b993aa5adad561a6a4764ab152bb30fd71d",
          "url": "https://github.com/ocaml/dune/commit/436965f550c40fd19fbe29d8a4b19f9b75bf8914"
        },
        "date": 1677899233923,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.7787683678533333",
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
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "distinct": true,
          "id": "a658bd6c754a9ad2a55c01ea92fc0e3dea942557",
          "message": "fix: disallow nested melange.emit stanzas\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-05T20:01:06-08:00",
          "tree_id": "94aa0c568f7726f771a66960b236c637e218bafe",
          "url": "https://github.com/ocaml/dune/commit/a658bd6c754a9ad2a55c01ea92fc0e3dea942557"
        },
        "date": 1678077340615,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "66.00451846413334",
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
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "distinct": true,
          "id": "a658bd6c754a9ad2a55c01ea92fc0e3dea942557",
          "message": "fix: disallow nested melange.emit stanzas\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-05T20:01:06-08:00",
          "tree_id": "94aa0c568f7726f771a66960b236c637e218bafe",
          "url": "https://github.com/ocaml/dune/commit/a658bd6c754a9ad2a55c01ea92fc0e3dea942557"
        },
        "date": 1678077351148,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.5985992628000003",
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
          "id": "77489f7cd89ee362fa7475da91aaca1b400ddce0",
          "message": "test: remove unnecessary shell script (#7209)\n\ninstead of sdune, just export an environment variable to enable\r\nsandboxing\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-07T07:04:03-08:00",
          "tree_id": "9cb2bbecc04d3ce663ce9b598884de0ab98f3173",
          "url": "https://github.com/ocaml/dune/commit/77489f7cd89ee362fa7475da91aaca1b400ddce0"
        },
        "date": 1678202916506,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "65.40419259372001",
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
          "id": "77489f7cd89ee362fa7475da91aaca1b400ddce0",
          "message": "test: remove unnecessary shell script (#7209)\n\ninstead of sdune, just export an environment variable to enable\r\nsandboxing\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-07T07:04:03-08:00",
          "tree_id": "9cb2bbecc04d3ce663ce9b598884de0ab98f3173",
          "url": "https://github.com/ocaml/dune/commit/77489f7cd89ee362fa7475da91aaca1b400ddce0"
        },
        "date": 1678202927145,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.6158967902466668",
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
          "id": "c28016854046cd2bdbf521939d5eae4a9eadc96c",
          "message": "feat(melange): add `runtime_deps` to copy assets to the target dir (#7199)\n\n* feat(melange): add `runtime_deps` to copy assets to the target dir\r\n\r\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-07T08:00:57-08:00",
          "tree_id": "88b6219604d85faf5ed2eb7c16edf6446b29bc88",
          "url": "https://github.com/ocaml/dune/commit/c28016854046cd2bdbf521939d5eae4a9eadc96c"
        },
        "date": 1678206789307,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "87.86102508745999",
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
          "id": "c28016854046cd2bdbf521939d5eae4a9eadc96c",
          "message": "feat(melange): add `runtime_deps` to copy assets to the target dir (#7199)\n\n* feat(melange): add `runtime_deps` to copy assets to the target dir\r\n\r\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-07T08:00:57-08:00",
          "tree_id": "88b6219604d85faf5ed2eb7c16edf6446b29bc88",
          "url": "https://github.com/ocaml/dune/commit/c28016854046cd2bdbf521939d5eae4a9eadc96c"
        },
        "date": 1678206803453,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "2.23607095668",
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
          "id": "ac0f50314c89f9da4e53310505faf4e21e6f9fc6",
          "message": "feature: use clonefile on macos (#7210)\n\nUse mac's [clonefile] instead of manually copying. [clonefile] is like\r\nhardlink but it will copy-on-write when edited.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-07T08:38:03-08:00",
          "tree_id": "05d8958bf2f214d3bd6b05e15788815ed0418e95",
          "url": "https://github.com/ocaml/dune/commit/ac0f50314c89f9da4e53310505faf4e21e6f9fc6"
        },
        "date": 1678208540132,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "64.99094837290666",
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
          "id": "ac0f50314c89f9da4e53310505faf4e21e6f9fc6",
          "message": "feature: use clonefile on macos (#7210)\n\nUse mac's [clonefile] instead of manually copying. [clonefile] is like\r\nhardlink but it will copy-on-write when edited.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-07T08:38:03-08:00",
          "tree_id": "05d8958bf2f214d3bd6b05e15788815ed0418e95",
          "url": "https://github.com/ocaml/dune/commit/ac0f50314c89f9da4e53310505faf4e21e6f9fc6"
        },
        "date": 1678208550596,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.5870201785933336",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "ivg@ieee.org",
            "name": "Ivan Gotovchits",
            "username": "ivg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "086a78cc56b25a4da5795671698d7ef4d1473a21",
          "message": "adds support for loading plugins in toplevels (#6082)\n\n* adds support for loading plugins in toplevels\r\n\r\nUses virtual libraries to select the proper dynamic linker\r\nfacility. To load in the toplevel add `dune-site.toplevel` library as\r\nthe dependency to your toplevel.\r\n\r\n3. Also, handle moved load_file function.  Prior to OCaml 4.13.0, the\r\nload_file function was in Topdirs.  Starting with OCaml 4.13.0, the\r\nload_file function moved to Toploop. In order to find it open both\r\nthese modules, suppressing the warning for unused open, and then\r\nreference load_file unqualified.\r\n\r\nSigned-off-by: Richard L Ford <richardlford@gmail.com>\r\nSigned-off-by: ivg <ivg@ieee.org>\r\nCo-authored-by: Richard L Ford <richardlford@gmail.com>",
          "timestamp": "2023-03-07T10:56:52-08:00",
          "tree_id": "4d0b1d0bf8c7d9c57a95f271146f9dc6c992e502",
          "url": "https://github.com/ocaml/dune/commit/086a78cc56b25a4da5795671698d7ef4d1473a21"
        },
        "date": 1678217138368,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "78.63035834904",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "ivg@ieee.org",
            "name": "Ivan Gotovchits",
            "username": "ivg"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "086a78cc56b25a4da5795671698d7ef4d1473a21",
          "message": "adds support for loading plugins in toplevels (#6082)\n\n* adds support for loading plugins in toplevels\r\n\r\nUses virtual libraries to select the proper dynamic linker\r\nfacility. To load in the toplevel add `dune-site.toplevel` library as\r\nthe dependency to your toplevel.\r\n\r\n3. Also, handle moved load_file function.  Prior to OCaml 4.13.0, the\r\nload_file function was in Topdirs.  Starting with OCaml 4.13.0, the\r\nload_file function moved to Toploop. In order to find it open both\r\nthese modules, suppressing the warning for unused open, and then\r\nreference load_file unqualified.\r\n\r\nSigned-off-by: Richard L Ford <richardlford@gmail.com>\r\nSigned-off-by: ivg <ivg@ieee.org>\r\nCo-authored-by: Richard L Ford <richardlford@gmail.com>",
          "timestamp": "2023-03-07T10:56:52-08:00",
          "tree_id": "4d0b1d0bf8c7d9c57a95f271146f9dc6c992e502",
          "url": "https://github.com/ocaml/dune/commit/086a78cc56b25a4da5795671698d7ef4d1473a21"
        },
        "date": 1678217152462,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.9827855113866668",
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
          "id": "a0dd51512f269d7c18f0c6216ab66d3ab7368da2",
          "message": "chore: add clonefile on macos optimization to CHANGES (#7237)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-07T11:45:52-08:00",
          "tree_id": "b471c4175c60ce05265ef19667bad2d9238c68fc",
          "url": "https://github.com/ocaml/dune/commit/a0dd51512f269d7c18f0c6216ab66d3ab7368da2"
        },
        "date": 1678219828123,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "65.9944071341",
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
          "id": "a0dd51512f269d7c18f0c6216ab66d3ab7368da2",
          "message": "chore: add clonefile on macos optimization to CHANGES (#7237)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-07T11:45:52-08:00",
          "tree_id": "b471c4175c60ce05265ef19667bad2d9238c68fc",
          "url": "https://github.com/ocaml/dune/commit/a0dd51512f269d7c18f0c6216ab66d3ab7368da2"
        },
        "date": 1678219838322,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.6045589385133334",
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
          "id": "a3ff6bf48bb5e203bfda1afaf1fe0b69ae77103d",
          "message": "Show that multiple licenses are supported (#7098)\n\nThe stanza is in the singular, but in fact supports more than one\r\nlicense. A lot of `.opam.template` files exist that add a `license`\r\nfield to be able to specify multiple licenses where this is not strictly\r\nrequired.\r\n\r\nSigned-off-by: Marek Kubica <marek@tarides.com>",
          "timestamp": "2023-03-08T14:46:55Z",
          "tree_id": "05a7593b486388c1fb85169ce2d7e9d232e0f076",
          "url": "https://github.com/ocaml/dune/commit/a3ff6bf48bb5e203bfda1afaf1fe0b69ae77103d"
        },
        "date": 1678288314951,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "67.87545671100001",
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
          "id": "a3ff6bf48bb5e203bfda1afaf1fe0b69ae77103d",
          "message": "Show that multiple licenses are supported (#7098)\n\nThe stanza is in the singular, but in fact supports more than one\r\nlicense. A lot of `.opam.template` files exist that add a `license`\r\nfield to be able to specify multiple licenses where this is not strictly\r\nrequired.\r\n\r\nSigned-off-by: Marek Kubica <marek@tarides.com>",
          "timestamp": "2023-03-08T14:46:55Z",
          "tree_id": "05a7593b486388c1fb85169ce2d7e9d232e0f076",
          "url": "https://github.com/ocaml/dune/commit/a3ff6bf48bb5e203bfda1afaf1fe0b69ae77103d"
        },
        "date": 1678288326127,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.6532052712666667",
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
          "id": "16b7df37807be538ba33adc5b7d55888c589205e",
          "message": "test: fix macOS test suite (#7236)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-08T11:51:03-08:00",
          "tree_id": "07ac0d15890807f368fde05aa4c7d71ccfefa501",
          "url": "https://github.com/ocaml/dune/commit/16b7df37807be538ba33adc5b7d55888c589205e"
        },
        "date": 1678306574744,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "69.79490539173334",
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
          "id": "16b7df37807be538ba33adc5b7d55888c589205e",
          "message": "test: fix macOS test suite (#7236)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-08T11:51:03-08:00",
          "tree_id": "07ac0d15890807f368fde05aa4c7d71ccfefa501",
          "url": "https://github.com/ocaml/dune/commit/16b7df37807be538ba33adc5b7d55888c589205e"
        },
        "date": 1678306586146,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.8143357543066667",
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
          "id": "3b934509f88b996d5118fe394e378514544d1494",
          "message": "test: sigpipe handling (#7242)\n\nWe show that sigpipe currently kills dune\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-08T12:04:18-08:00",
          "tree_id": "e5fe1bc41931c1d1984c5f3effdef92a8166c7d1",
          "url": "https://github.com/ocaml/dune/commit/3b934509f88b996d5118fe394e378514544d1494"
        },
        "date": 1678307637114,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "81.54937648955999",
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
          "id": "3b934509f88b996d5118fe394e378514544d1494",
          "message": "test: sigpipe handling (#7242)\n\nWe show that sigpipe currently kills dune\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-08T12:04:18-08:00",
          "tree_id": "e5fe1bc41931c1d1984c5f3effdef92a8166c7d1",
          "url": "https://github.com/ocaml/dune/commit/3b934509f88b996d5118fe394e378514544d1494"
        },
        "date": 1678307650795,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "2.0800937332066667",
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
          "id": "ebf90a3fdefc16604a97df3a627d7c33b68a48c4",
          "message": "coq_config: add dyn and improve comments\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>\n\n<!-- ps-id: 9bb3f6fb-281a-47e3-b7fb-138d85becd81 -->",
          "timestamp": "2023-03-08T21:22:00+01:00",
          "tree_id": "cafadc3013ac8cae4c0fb5240bf65c0013213217",
          "url": "https://github.com/ocaml/dune/commit/ebf90a3fdefc16604a97df3a627d7c33b68a48c4"
        },
        "date": 1678308454016,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "67.69058430788",
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
          "id": "ebf90a3fdefc16604a97df3a627d7c33b68a48c4",
          "message": "coq_config: add dyn and improve comments\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>\n\n<!-- ps-id: 9bb3f6fb-281a-47e3-b7fb-138d85becd81 -->",
          "timestamp": "2023-03-08T21:22:00+01:00",
          "tree_id": "cafadc3013ac8cae4c0fb5240bf65c0013213217",
          "url": "https://github.com/ocaml/dune/commit/ebf90a3fdefc16604a97df3a627d7c33b68a48c4"
        },
        "date": 1678308465003,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.6201841642333334",
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
          "id": "f60f7216dcfd3bae9d37ddfdca042278bb9a5229",
          "message": "loc: make pp_file_colon_line polymorphic (#7243)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-08T12:18:37-08:00",
          "tree_id": "5d6ba820ccaa5920e72f2a7ede236bf58f5522f8",
          "url": "https://github.com/ocaml/dune/commit/f60f7216dcfd3bae9d37ddfdca042278bb9a5229"
        },
        "date": 1678308525319,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "76.25120478217333",
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
          "id": "f60f7216dcfd3bae9d37ddfdca042278bb9a5229",
          "message": "loc: make pp_file_colon_line polymorphic (#7243)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-08T12:18:37-08:00",
          "tree_id": "5d6ba820ccaa5920e72f2a7ede236bf58f5522f8",
          "url": "https://github.com/ocaml/dune/commit/f60f7216dcfd3bae9d37ddfdca042278bb9a5229"
        },
        "date": 1678308539148,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.9703622445666664",
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
          "id": "3802efe8100508bb60eb599e0a105f4556c887d5",
          "message": "coq_config: Add coqcorelib\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>\n\n<!-- ps-id: 1d5e63eb-4cb5-466f-8270-bf05bd1fe056 -->",
          "timestamp": "2023-03-08T21:42:14+01:00",
          "tree_id": "431953a65bb9985cb86ff5e44ea820fd4dd83293",
          "url": "https://github.com/ocaml/dune/commit/3802efe8100508bb60eb599e0a105f4556c887d5"
        },
        "date": 1678309906711,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "81.70524991299332",
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
          "id": "3802efe8100508bb60eb599e0a105f4556c887d5",
          "message": "coq_config: Add coqcorelib\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>\n\n<!-- ps-id: 1d5e63eb-4cb5-466f-8270-bf05bd1fe056 -->",
          "timestamp": "2023-03-08T21:42:14+01:00",
          "tree_id": "431953a65bb9985cb86ff5e44ea820fd4dd83293",
          "url": "https://github.com/ocaml/dune/commit/3802efe8100508bb60eb599e0a105f4556c887d5"
        },
        "date": 1678309919880,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "2.0131303777266667",
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
          "id": "357348710e9c00dd5a8cea6074b9187e8e2a5e77",
          "message": "ci(coq): bump coq version to 8.16.1\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>\n\n<!-- ps-id: 90edb35a-a48c-4b1a-99f5-ef48b01352ae -->",
          "timestamp": "2023-03-08T22:06:41+01:00",
          "tree_id": "8084c0cf8cc1a44895ec8891955d8ea384b650b1",
          "url": "https://github.com/ocaml/dune/commit/357348710e9c00dd5a8cea6074b9187e8e2a5e77"
        },
        "date": 1678311535455,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "88.00237454479333",
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
          "id": "357348710e9c00dd5a8cea6074b9187e8e2a5e77",
          "message": "ci(coq): bump coq version to 8.16.1\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>\n\n<!-- ps-id: 90edb35a-a48c-4b1a-99f5-ef48b01352ae -->",
          "timestamp": "2023-03-08T22:06:41+01:00",
          "tree_id": "8084c0cf8cc1a44895ec8891955d8ea384b650b1",
          "url": "https://github.com/ocaml/dune/commit/357348710e9c00dd5a8cea6074b9187e8e2a5e77"
        },
        "date": 1678311549851,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "2.2556420041666665",
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
          "id": "634bc0ebbf9f85dddb849c065a2d9caf4e7a3071",
          "message": "test(coq): duplicate theory in a project\n\nAppears to work correctly.\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>\n\n<!-- ps-id: 45dfb6a8-bff8-48ae-8ad7-fa0ca6b41d12 -->",
          "timestamp": "2023-03-08T23:20:00+01:00",
          "tree_id": "91a11ed7cde64337ffc350ef7fe43244fefefcf0",
          "url": "https://github.com/ocaml/dune/commit/634bc0ebbf9f85dddb849c065a2d9caf4e7a3071"
        },
        "date": 1678316051097,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "81.64070325075333",
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
          "id": "634bc0ebbf9f85dddb849c065a2d9caf4e7a3071",
          "message": "test(coq): duplicate theory in a project\n\nAppears to work correctly.\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>\n\n<!-- ps-id: 45dfb6a8-bff8-48ae-8ad7-fa0ca6b41d12 -->",
          "timestamp": "2023-03-08T23:20:00+01:00",
          "tree_id": "91a11ed7cde64337ffc350ef7fe43244fefefcf0",
          "url": "https://github.com/ocaml/dune/commit/634bc0ebbf9f85dddb849c065a2d9caf4e7a3071"
        },
        "date": 1678316064552,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "2.1275339876133335",
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
          "id": "44c01c1a413b700d4297efa83fc56c3e78b2c03d",
          "message": "refactor: use `Pp.enumerate` in 2 places (#7252)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-08T15:29:58-08:00",
          "tree_id": "4992d73585e05be46f12e0c49e654bb246f2a338",
          "url": "https://github.com/ocaml/dune/commit/44c01c1a413b700d4297efa83fc56c3e78b2c03d"
        },
        "date": 1678319645075,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "64.2412970892",
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
          "id": "44c01c1a413b700d4297efa83fc56c3e78b2c03d",
          "message": "refactor: use `Pp.enumerate` in 2 places (#7252)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-08T15:29:58-08:00",
          "tree_id": "4992d73585e05be46f12e0c49e654bb246f2a338",
          "url": "https://github.com/ocaml/dune/commit/44c01c1a413b700d4297efa83fc56c3e78b2c03d"
        },
        "date": 1678319655095,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.571331639026667",
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
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "distinct": true,
          "id": "c7a0049e24e6d802a46f7ed34abf42bc525bf89d",
          "message": "fix(melange): two emit stanzas in a directory\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-08T15:31:10-08:00",
          "tree_id": "10156b26142855e6044ee31b2f2131bae98be703",
          "url": "https://github.com/ocaml/dune/commit/c7a0049e24e6d802a46f7ed34abf42bc525bf89d"
        },
        "date": 1678320038694,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "82.51484754191334",
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
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "distinct": true,
          "id": "c7a0049e24e6d802a46f7ed34abf42bc525bf89d",
          "message": "fix(melange): two emit stanzas in a directory\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-08T15:31:10-08:00",
          "tree_id": "10156b26142855e6044ee31b2f2131bae98be703",
          "url": "https://github.com/ocaml/dune/commit/c7a0049e24e6d802a46f7ed34abf42bc525bf89d"
        },
        "date": 1678320051968,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "2.0822602418000002",
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
          "id": "d429530c59f13ffdee023bc78ee607ac9850ce96",
          "message": "feat(melange): support `(select ...)` in `melange.emit` (#7239)\n\n* feat(melange): support `(select ...)` in `melange.emit`\r\n\r\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-08T17:06:25-08:00",
          "tree_id": "f5e91d65b8b2b70979230785ddcf7ef8d3bd8d14",
          "url": "https://github.com/ocaml/dune/commit/d429530c59f13ffdee023bc78ee607ac9850ce96"
        },
        "date": 1678325559867,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "73.42887934693334",
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
          "id": "d429530c59f13ffdee023bc78ee607ac9850ce96",
          "message": "feat(melange): support `(select ...)` in `melange.emit` (#7239)\n\n* feat(melange): support `(select ...)` in `melange.emit`\r\n\r\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-08T17:06:25-08:00",
          "tree_id": "f5e91d65b8b2b70979230785ddcf7ef8d3bd8d14",
          "url": "https://github.com/ocaml/dune/commit/d429530c59f13ffdee023bc78ee607ac9850ce96"
        },
        "date": 1678325571503,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.8628842621400004",
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
          "id": "3e99fc556abbedee00a95a0e38bddf5ab8980ac8",
          "message": "test(memo): demonstrate loss of concurrency (#7251)\n\nReproduces the loss of concurrency observed in #5549 in a unit test\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-09T03:35:08Z",
          "tree_id": "37f08e73bd140fe5885838d3c04a62db51c35199",
          "url": "https://github.com/ocaml/dune/commit/3e99fc556abbedee00a95a0e38bddf5ab8980ac8"
        },
        "date": 1678334388532,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "66.07233983574001",
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
          "id": "3e99fc556abbedee00a95a0e38bddf5ab8980ac8",
          "message": "test(memo): demonstrate loss of concurrency (#7251)\n\nReproduces the loss of concurrency observed in #5549 in a unit test\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-09T03:35:08Z",
          "tree_id": "37f08e73bd140fe5885838d3c04a62db51c35199",
          "url": "https://github.com/ocaml/dune/commit/3e99fc556abbedee00a95a0e38bddf5ab8980ac8"
        },
        "date": 1678334399000,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.6147009206533334",
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
          "id": "c82312b020bcee6f047ab588691ae02d0f0a5bf7",
          "message": "chore: fix melange in the main default devShell (#7256)\n\n* chore: fix melange in the main default devShell\r\n* fix: simplify flake, get dune version from the melange overlay\r\n\r\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-09T13:50:47-08:00",
          "tree_id": "d3407acc89f49bb9e0c4c8d7d9b318d27a1a146f",
          "url": "https://github.com/ocaml/dune/commit/c82312b020bcee6f047ab588691ae02d0f0a5bf7"
        },
        "date": 1678400987121,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "86.98200649479334",
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
          "id": "c82312b020bcee6f047ab588691ae02d0f0a5bf7",
          "message": "chore: fix melange in the main default devShell (#7256)\n\n* chore: fix melange in the main default devShell\r\n* fix: simplify flake, get dune version from the melange overlay\r\n\r\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-09T13:50:47-08:00",
          "tree_id": "d3407acc89f49bb9e0c4c8d7d9b318d27a1a146f",
          "url": "https://github.com/ocaml/dune/commit/c82312b020bcee6f047ab588691ae02d0f0a5bf7"
        },
        "date": 1678401002317,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "2.3803777479600003",
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
          "id": "d5582c7d677b0d8afcc92f5c390d014cdac1a97f",
          "message": "chore(nix): remove the opam2nix dependency on the default package (#7258)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-09T15:09:35-08:00",
          "tree_id": "0e579760f62fdb226997e861522d4c545a9d5816",
          "url": "https://github.com/ocaml/dune/commit/d5582c7d677b0d8afcc92f5c390d014cdac1a97f"
        },
        "date": 1678404861816,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "66.21187931645332",
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
          "id": "d5582c7d677b0d8afcc92f5c390d014cdac1a97f",
          "message": "chore(nix): remove the opam2nix dependency on the default package (#7258)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-09T15:09:35-08:00",
          "tree_id": "0e579760f62fdb226997e861522d4c545a9d5816",
          "url": "https://github.com/ocaml/dune/commit/d5582c7d677b0d8afcc92f5c390d014cdac1a97f"
        },
        "date": 1678404872386,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.5951990953",
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
          "id": "8e1567dd0ed31ad8fadb32df87e36cc65a0928e8",
          "message": "coq: improve directory target message (#7250)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-10T08:42:30-08:00",
          "tree_id": "97da383ba2b1bffa21d44ff73989a7de6e9d8bb0",
          "url": "https://github.com/ocaml/dune/commit/8e1567dd0ed31ad8fadb32df87e36cc65a0928e8"
        },
        "date": 1678468723084,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "64.59079321679333",
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
          "id": "8e1567dd0ed31ad8fadb32df87e36cc65a0928e8",
          "message": "coq: improve directory target message (#7250)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-10T08:42:30-08:00",
          "tree_id": "97da383ba2b1bffa21d44ff73989a7de6e9d8bb0",
          "url": "https://github.com/ocaml/dune/commit/8e1567dd0ed31ad8fadb32df87e36cc65a0928e8"
        },
        "date": 1678468732902,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.56740791714",
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
          "id": "e52c89bc2939982426b2f3dc27d1e04631f76d01",
          "message": "test(coq): duplicate theory in a dune file\n\nAppears to be bugged since the directory targets are being reported\nfirst.\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>\n\n<!-- ps-id: 028d7bfb-988a-44e2-ae57-72fcfeee8d5f -->",
          "timestamp": "2023-03-10T17:51:32+01:00",
          "tree_id": "6ea29e142e086ce099e456b41e048e639b9678dd",
          "url": "https://github.com/ocaml/dune/commit/e52c89bc2939982426b2f3dc27d1e04631f76d01"
        },
        "date": 1678469087830,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "66.88364988725334",
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
          "id": "e52c89bc2939982426b2f3dc27d1e04631f76d01",
          "message": "test(coq): duplicate theory in a dune file\n\nAppears to be bugged since the directory targets are being reported\nfirst.\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>\n\n<!-- ps-id: 028d7bfb-988a-44e2-ae57-72fcfeee8d5f -->",
          "timestamp": "2023-03-10T17:51:32+01:00",
          "tree_id": "6ea29e142e086ce099e456b41e048e639b9678dd",
          "url": "https://github.com/ocaml/dune/commit/e52c89bc2939982426b2f3dc27d1e04631f76d01"
        },
        "date": 1678469098238,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.614109424506667",
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
          "id": "eec2b08d68cfe8408e662b8e5e897d2c0b8d366d",
          "message": "fix(coq): fix duplicate dir targets being detected before theories (#7263)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-10T09:40:51-08:00",
          "tree_id": "286b637bce1c9d866d72a50b6822baa53ea1b01c",
          "url": "https://github.com/ocaml/dune/commit/eec2b08d68cfe8408e662b8e5e897d2c0b8d366d"
        },
        "date": 1678471651730,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "73.76795251687334",
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
          "id": "eec2b08d68cfe8408e662b8e5e897d2c0b8d366d",
          "message": "fix(coq): fix duplicate dir targets being detected before theories (#7263)\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-10T09:40:51-08:00",
          "tree_id": "286b637bce1c9d866d72a50b6822baa53ea1b01c",
          "url": "https://github.com/ocaml/dune/commit/eec2b08d68cfe8408e662b8e5e897d2c0b8d366d"
        },
        "date": 1678471663363,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.91418740344",
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
          "id": "4c696272ad42bbb3f3f6603f52571b7cd92b8e3e",
          "message": "chore(makefile): update OCaml to 4.14.1 (#7265)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-10T12:17:35-08:00",
          "tree_id": "710714f31ae25e9ff9baa69d5e08558073d838af",
          "url": "https://github.com/ocaml/dune/commit/4c696272ad42bbb3f3f6603f52571b7cd92b8e3e"
        },
        "date": 1678481255986,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "81.69595629974002",
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
          "id": "4c696272ad42bbb3f3f6603f52571b7cd92b8e3e",
          "message": "chore(makefile): update OCaml to 4.14.1 (#7265)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-10T12:17:35-08:00",
          "tree_id": "710714f31ae25e9ff9baa69d5e08558073d838af",
          "url": "https://github.com/ocaml/dune/commit/4c696272ad42bbb3f3f6603f52571b7cd92b8e3e"
        },
        "date": 1678481269601,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "2.129604609413333",
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
          "id": "82df8daf476d615681061b9d513710d2cb16daf6",
          "message": "chore(docker): add perf to image (#7266)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-10T12:44:09-08:00",
          "tree_id": "583fd28cd16e279964412f91f9e7b91fda8c4437",
          "url": "https://github.com/ocaml/dune/commit/82df8daf476d615681061b9d513710d2cb16daf6"
        },
        "date": 1678484219961,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "81.94108803015997",
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
          "id": "82df8daf476d615681061b9d513710d2cb16daf6",
          "message": "chore(docker): add perf to image (#7266)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-10T12:44:09-08:00",
          "tree_id": "583fd28cd16e279964412f91f9e7b91fda8c4437",
          "url": "https://github.com/ocaml/dune/commit/82df8daf476d615681061b9d513710d2cb16daf6"
        },
        "date": 1678484233985,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "2.1372471224800003",
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
          "id": "0cff92b585fd5edeca88a6c70d2134e2266bd89e",
          "message": "chore(docker): preserve history between sessions (#7269)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-10T16:27:54-08:00",
          "tree_id": "5760bcf178d2d2aeba545c34d07fcf952ab251d6",
          "url": "https://github.com/ocaml/dune/commit/0cff92b585fd5edeca88a6c70d2134e2266bd89e"
        },
        "date": 1678495987806,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "70.42624078760001",
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
          "id": "0cff92b585fd5edeca88a6c70d2134e2266bd89e",
          "message": "chore(docker): preserve history between sessions (#7269)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-10T16:27:54-08:00",
          "tree_id": "5760bcf178d2d2aeba545c34d07fcf952ab251d6",
          "url": "https://github.com/ocaml/dune/commit/0cff92b585fd5edeca88a6c70d2134e2266bd89e"
        },
        "date": 1678495998833,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.8046650195933334",
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
          "id": "1402ef79b486ec740adecd1572c8e421146cb9c4",
          "message": "fix(boot): fix env vars when finding out concurrency (#7272)\n\nWe were passing an empty enviornment to Unix.open_process_full_cmd\r\nbefore. We now pass the full available Unix.environment since we are\r\nonly querying the concurrency by calling nproc or getconf.\r\n\r\nOn NixOS this was causing the bootstrap to default to a single\r\nprocessor since nproc wasn't available in the empty environment.\r\n\r\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-10T16:45:05-08:00",
          "tree_id": "627252095722073f3c929ba8841c945b7f494bb7",
          "url": "https://github.com/ocaml/dune/commit/1402ef79b486ec740adecd1572c8e421146cb9c4"
        },
        "date": 1678496943627,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "64.61467923599334",
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
          "id": "1402ef79b486ec740adecd1572c8e421146cb9c4",
          "message": "fix(boot): fix env vars when finding out concurrency (#7272)\n\nWe were passing an empty enviornment to Unix.open_process_full_cmd\r\nbefore. We now pass the full available Unix.environment since we are\r\nonly querying the concurrency by calling nproc or getconf.\r\n\r\nOn NixOS this was causing the bootstrap to default to a single\r\nprocessor since nproc wasn't available in the empty environment.\r\n\r\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-10T16:45:05-08:00",
          "tree_id": "627252095722073f3c929ba8841c945b7f494bb7",
          "url": "https://github.com/ocaml/dune/commit/1402ef79b486ec740adecd1572c8e421146cb9c4"
        },
        "date": 1678496953779,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.5720029893066665",
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
          "id": "57c8e78875d6ca90bc050bc5c6ebf52094f20953",
          "message": "feature(boot): remove reliance on shell (#7274)\n\nPreviously, we'd run the shell to discover the number of processes by\r\nusing `Unix.open_process_full`. The shell isn't being used for anything,\r\nso we switch to executing the process directly.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-10T18:39:26-08:00",
          "tree_id": "9a47839dbcec4fe5e2c7a7ec5b33e7b6576cde21",
          "url": "https://github.com/ocaml/dune/commit/57c8e78875d6ca90bc050bc5c6ebf52094f20953"
        },
        "date": 1678503822935,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "64.53667394314668",
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
          "id": "57c8e78875d6ca90bc050bc5c6ebf52094f20953",
          "message": "feature(boot): remove reliance on shell (#7274)\n\nPreviously, we'd run the shell to discover the number of processes by\r\nusing `Unix.open_process_full`. The shell isn't being used for anything,\r\nso we switch to executing the process directly.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-10T18:39:26-08:00",
          "tree_id": "9a47839dbcec4fe5e2c7a7ec5b33e7b6576cde21",
          "url": "https://github.com/ocaml/dune/commit/57c8e78875d6ca90bc050bc5c6ebf52094f20953"
        },
        "date": 1678503833669,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.5624546020733334",
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
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "distinct": true,
          "id": "1260dc53c1b3fcdb1cee8c8c6d9c2209a5e59428",
          "message": "fix(Io.copy_file): fail fast if src is a directory\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-10T21:56:56-08:00",
          "tree_id": "167f0a33811644e36b1303ef53e498f3e04a596f",
          "url": "https://github.com/ocaml/dune/commit/1260dc53c1b3fcdb1cee8c8c6d9c2209a5e59428"
        },
        "date": 1678515683996,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "64.38469641982",
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
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "distinct": true,
          "id": "1260dc53c1b3fcdb1cee8c8c6d9c2209a5e59428",
          "message": "fix(Io.copy_file): fail fast if src is a directory\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-10T21:56:56-08:00",
          "tree_id": "167f0a33811644e36b1303ef53e498f3e04a596f",
          "url": "https://github.com/ocaml/dune/commit/1260dc53c1b3fcdb1cee8c8c6d9c2209a5e59428"
        },
        "date": 1678515694452,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.5547914916333336",
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
          "id": "a3bbd92ef265423de4c05371ea20576b974935e1",
          "message": "refactor(stdune.io): share directory check (#7278)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-10T22:09:41-08:00",
          "tree_id": "be8f37723ee34ec640bcfb1ee15842d9178f33b4",
          "url": "https://github.com/ocaml/dune/commit/a3bbd92ef265423de4c05371ea20576b974935e1"
        },
        "date": 1678516806607,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "80.87326451754001",
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
          "id": "a3bbd92ef265423de4c05371ea20576b974935e1",
          "message": "refactor(stdune.io): share directory check (#7278)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-10T22:09:41-08:00",
          "tree_id": "be8f37723ee34ec640bcfb1ee15842d9178f33b4",
          "url": "https://github.com/ocaml/dune/commit/a3bbd92ef265423de4c05371ea20576b974935e1"
        },
        "date": 1678516819938,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "2.09396534256",
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
          "id": "50387d3bc8f095ae4ea0d681de775a46580c5899",
          "message": "chore(nix): share test deps in all shells (#7275)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-10T22:23:21-08:00",
          "tree_id": "2a6feeeb5777fbbddf5eadebb396436341d363c6",
          "url": "https://github.com/ocaml/dune/commit/50387d3bc8f095ae4ea0d681de775a46580c5899"
        },
        "date": 1678517304731,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "68.89209300547999",
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
          "id": "50387d3bc8f095ae4ea0d681de775a46580c5899",
          "message": "chore(nix): share test deps in all shells (#7275)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-10T22:23:21-08:00",
          "tree_id": "2a6feeeb5777fbbddf5eadebb396436341d363c6",
          "url": "https://github.com/ocaml/dune/commit/50387d3bc8f095ae4ea0d681de775a46580c5899"
        },
        "date": 1678517315472,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.7355588347200002",
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
          "id": "848a5e764f108dc31a595343eb18fc76462761d9",
          "message": "test: reproducibility of sigpipe.t (#7273)\n\nCreate the output file before running dune so that the final `cat` never\r\nfails.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>\r\n\r\n<!-- ps-id: f95e7a20-622a-4e24-be18-25f119d68ef8 -->",
          "timestamp": "2023-03-11T09:03:19-08:00",
          "tree_id": "972841937b2710114e0de4b1547740159aa98bc7",
          "url": "https://github.com/ocaml/dune/commit/848a5e764f108dc31a595343eb18fc76462761d9"
        },
        "date": 1678555684544,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "66.3336835869",
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
          "id": "848a5e764f108dc31a595343eb18fc76462761d9",
          "message": "test: reproducibility of sigpipe.t (#7273)\n\nCreate the output file before running dune so that the final `cat` never\r\nfails.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>\r\n\r\n<!-- ps-id: f95e7a20-622a-4e24-be18-25f119d68ef8 -->",
          "timestamp": "2023-03-11T09:03:19-08:00",
          "tree_id": "972841937b2710114e0de4b1547740159aa98bc7",
          "url": "https://github.com/ocaml/dune/commit/848a5e764f108dc31a595343eb18fc76462761d9"
        },
        "date": 1678555695352,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.6060317917000002",
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
          "id": "71a950fc2d7a01c75d1745ce364f2aa370a7ba47",
          "message": "doc: cleanup support seciton of readme and remove jbuilder mentions (#7283)\n\nIt has been quite a while since the jbuilder migration so I think we\r\ncan safely remove any mention of it in the README.md.\r\n\r\nI've also improved the support section pointing to GitHub discussions\r\nand issues. I am not sure the ocaml-core email is being checked by\r\nanybody so I have removed it.\r\n\r\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-11T09:08:16-08:00",
          "tree_id": "1b9f720fe9a374fa94b5ffe2d1db49a80fc33ecf",
          "url": "https://github.com/ocaml/dune/commit/71a950fc2d7a01c75d1745ce364f2aa370a7ba47"
        },
        "date": 1678555964503,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "65.78847153535999",
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
          "id": "71a950fc2d7a01c75d1745ce364f2aa370a7ba47",
          "message": "doc: cleanup support seciton of readme and remove jbuilder mentions (#7283)\n\nIt has been quite a while since the jbuilder migration so I think we\r\ncan safely remove any mention of it in the README.md.\r\n\r\nI've also improved the support section pointing to GitHub discussions\r\nand issues. I am not sure the ocaml-core email is being checked by\r\nanybody so I have removed it.\r\n\r\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-11T09:08:16-08:00",
          "tree_id": "1b9f720fe9a374fa94b5ffe2d1db49a80fc33ecf",
          "url": "https://github.com/ocaml/dune/commit/71a950fc2d7a01c75d1745ce364f2aa370a7ba47"
        },
        "date": 1678555975193,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.5975163261800003",
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
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "00cf75ae1612c9f0fd8f9d57c32446da3f12d38d",
          "message": "chore: pin but not install melange\n\nSo that we install melange along with the rest of the deps at once\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>\n\n<!-- ps-id: 88ca4ba3-3e2a-4b53-b303-0cb2b596faba -->",
          "timestamp": "2023-03-11T20:25:51+01:00",
          "tree_id": "83dedfacae91c8a45e23901c35f6477eb321ae88",
          "url": "https://github.com/ocaml/dune/commit/00cf75ae1612c9f0fd8f9d57c32446da3f12d38d"
        },
        "date": 1678564180554,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "64.86594959366",
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
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "00cf75ae1612c9f0fd8f9d57c32446da3f12d38d",
          "message": "chore: pin but not install melange\n\nSo that we install melange along with the rest of the deps at once\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>\n\n<!-- ps-id: 88ca4ba3-3e2a-4b53-b303-0cb2b596faba -->",
          "timestamp": "2023-03-11T20:25:51+01:00",
          "tree_id": "83dedfacae91c8a45e23901c35f6477eb321ae88",
          "url": "https://github.com/ocaml/dune/commit/00cf75ae1612c9f0fd8f9d57c32446da3f12d38d"
        },
        "date": 1678564191267,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.57036994856",
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
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "b3a59e514b290f7135ceb572377f6f0d700e38d0",
          "message": "fix(console): clean up console on exit\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>\n\n<!-- ps-id: 12e55e26-139f-4c14-a1c8-62a97001fbed -->",
          "timestamp": "2023-03-11T20:30:11+01:00",
          "tree_id": "25947e4e5a327fc348182d2199c16ff2f81c7e98",
          "url": "https://github.com/ocaml/dune/commit/b3a59e514b290f7135ceb572377f6f0d700e38d0"
        },
        "date": 1678564456609,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "64.41457876180667",
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
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "b3a59e514b290f7135ceb572377f6f0d700e38d0",
          "message": "fix(console): clean up console on exit\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>\n\n<!-- ps-id: 12e55e26-139f-4c14-a1c8-62a97001fbed -->",
          "timestamp": "2023-03-11T20:30:11+01:00",
          "tree_id": "25947e4e5a327fc348182d2199c16ff2f81c7e98",
          "url": "https://github.com/ocaml/dune/commit/b3a59e514b290f7135ceb572377f6f0d700e38d0"
        },
        "date": 1678564466954,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.5585839276266669",
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
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "67d714edfab99366e32ff3a809a8456c7ff078f3",
          "message": "fix(nix): fix duplicate packages warning\n\nafter https://github.com/NixOS/nixpkgs/pull/205646, Nix detects\nconflicting findlib libraries in the `$OCAMLPATH`. Because we use 2\nnixpkgs sources, patdiff was running into that case\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-11T22:15:28+01:00",
          "tree_id": "4728bb430367e557addd09b8083ccbf77f5babc5",
          "url": "https://github.com/ocaml/dune/commit/67d714edfab99366e32ff3a809a8456c7ff078f3"
        },
        "date": 1678570781795,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "67.12668165960666",
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
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "67d714edfab99366e32ff3a809a8456c7ff078f3",
          "message": "fix(nix): fix duplicate packages warning\n\nafter https://github.com/NixOS/nixpkgs/pull/205646, Nix detects\nconflicting findlib libraries in the `$OCAMLPATH`. Because we use 2\nnixpkgs sources, patdiff was running into that case\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-11T22:15:28+01:00",
          "tree_id": "4728bb430367e557addd09b8083ccbf77f5babc5",
          "url": "https://github.com/ocaml/dune/commit/67d714edfab99366e32ff3a809a8456c7ff078f3"
        },
        "date": 1678570792055,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.6244821570266668",
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
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "095d4a87e5e9d9acb176070fcb120ba1c8435bc7",
          "message": "fix(nix): conflicting ocamlfind dependencies\n\nIn #7258, we removed opam2nix from the `default` package, but the\ndefault shell needs to depend on the correct one (now dune rather than\ndefault)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-11T23:53:00+01:00",
          "tree_id": "2ef3707ca5f8b36d9a7a727e79f3a80ca46f6af0",
          "url": "https://github.com/ocaml/dune/commit/095d4a87e5e9d9acb176070fcb120ba1c8435bc7"
        },
        "date": 1678576670630,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "70.88113397128",
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
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "095d4a87e5e9d9acb176070fcb120ba1c8435bc7",
          "message": "fix(nix): conflicting ocamlfind dependencies\n\nIn #7258, we removed opam2nix from the `default` package, but the\ndefault shell needs to depend on the correct one (now dune rather than\ndefault)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-11T23:53:00+01:00",
          "tree_id": "2ef3707ca5f8b36d9a7a727e79f3a80ca46f6af0",
          "url": "https://github.com/ocaml/dune/commit/095d4a87e5e9d9acb176070fcb120ba1c8435bc7"
        },
        "date": 1678576682271,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.8278923634600002",
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
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "7a6974cf1b9faa055f22d72c7b39f3df13212337",
          "message": "chore: docs, consolidate rule streaming and production\n\nSigned-off-by: Javier Chávarri <javier.chavarri@gmail.com>",
          "timestamp": "2023-03-12T01:01:35+01:00",
          "tree_id": "48a94b5bfa9f6c778955e0f7fe0a5f25567faf63",
          "url": "https://github.com/ocaml/dune/commit/7a6974cf1b9faa055f22d72c7b39f3df13212337"
        },
        "date": 1678580841941,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "71.20090494588668",
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
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "7a6974cf1b9faa055f22d72c7b39f3df13212337",
          "message": "chore: docs, consolidate rule streaming and production\n\nSigned-off-by: Javier Chávarri <javier.chavarri@gmail.com>",
          "timestamp": "2023-03-12T01:01:35+01:00",
          "tree_id": "48a94b5bfa9f6c778955e0f7fe0a5f25567faf63",
          "url": "https://github.com/ocaml/dune/commit/7a6974cf1b9faa055f22d72c7b39f3df13212337"
        },
        "date": 1678580853300,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.8603575485333332",
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
          "id": "4a0efff885e4216ffca7aa3cecf7418a656fc230",
          "message": "nix: remove coq from the main flake\n\nCoq has its own flake anyway.\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>\n\n<!-- ps-id: 3663fd3e-0ef5-4686-8ab4-77c28706648f -->",
          "timestamp": "2023-03-12T01:03:27+01:00",
          "tree_id": "04e1c771bae133130f1fee98722f5841d013ce8f",
          "url": "https://github.com/ocaml/dune/commit/4a0efff885e4216ffca7aa3cecf7418a656fc230"
        },
        "date": 1678581171553,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "81.50571394216666",
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
          "id": "4a0efff885e4216ffca7aa3cecf7418a656fc230",
          "message": "nix: remove coq from the main flake\n\nCoq has its own flake anyway.\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>\n\n<!-- ps-id: 3663fd3e-0ef5-4686-8ab4-77c28706648f -->",
          "timestamp": "2023-03-12T01:03:27+01:00",
          "tree_id": "04e1c771bae133130f1fee98722f5841d013ce8f",
          "url": "https://github.com/ocaml/dune/commit/4a0efff885e4216ffca7aa3cecf7418a656fc230"
        },
        "date": 1678581185223,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "2.1695339775333333",
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
          "id": "c5cadcf535fe099aa5cdc323f064a97cbd47e4c6",
          "message": "nix: add ocamlformat to testNativeBuildInputs in flake\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>\n\n<!-- ps-id: 1b96f5c7-fea3-4927-a48b-1ccf8f009e8d -->",
          "timestamp": "2023-03-12T01:25:02+01:00",
          "tree_id": "4427ca414e329a846423b5c2df9318e0a966572b",
          "url": "https://github.com/ocaml/dune/commit/c5cadcf535fe099aa5cdc323f064a97cbd47e4c6"
        },
        "date": 1678582236184,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "71.82194187894002",
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
          "id": "54e6ce1ebaac4b5854d60bf0e17874d777cd4fbf",
          "message": "nix: add formatter\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>\n\n<!-- ps-id: ec5d9ef8-d1fc-443b-93fb-c9670bda26c9 -->",
          "timestamp": "2023-03-12T01:24:43+01:00",
          "tree_id": "6f7701da24eb6a251918beed04c050518b226a70",
          "url": "https://github.com/ocaml/dune/commit/54e6ce1ebaac4b5854d60bf0e17874d777cd4fbf"
        },
        "date": 1678582239319,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "70.26215380919334",
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
          "id": "c5cadcf535fe099aa5cdc323f064a97cbd47e4c6",
          "message": "nix: add ocamlformat to testNativeBuildInputs in flake\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>\n\n<!-- ps-id: 1b96f5c7-fea3-4927-a48b-1ccf8f009e8d -->",
          "timestamp": "2023-03-12T01:25:02+01:00",
          "tree_id": "4427ca414e329a846423b5c2df9318e0a966572b",
          "url": "https://github.com/ocaml/dune/commit/c5cadcf535fe099aa5cdc323f064a97cbd47e4c6"
        },
        "date": 1678582247408,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.8346272623733333",
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
          "id": "54e6ce1ebaac4b5854d60bf0e17874d777cd4fbf",
          "message": "nix: add formatter\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>\n\n<!-- ps-id: ec5d9ef8-d1fc-443b-93fb-c9670bda26c9 -->",
          "timestamp": "2023-03-12T01:24:43+01:00",
          "tree_id": "6f7701da24eb6a251918beed04c050518b226a70",
          "url": "https://github.com/ocaml/dune/commit/54e6ce1ebaac4b5854d60bf0e17874d777cd4fbf"
        },
        "date": 1678582251152,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.8162821245733332",
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
          "id": "f9a0e6f00bfc731df60f44a7544eecc7dfaa7778",
          "message": "test(stdune): more copying tests (#7284)\n\ncover more error cases:\r\n- missing src\r\n- src is a directory\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-11T16:30:24-08:00",
          "tree_id": "faa0dc353332556f05e605cd937e0aec1af2655e",
          "url": "https://github.com/ocaml/dune/commit/f9a0e6f00bfc731df60f44a7544eecc7dfaa7778"
        },
        "date": 1678582486822,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "69.31656978473332",
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
          "id": "f9a0e6f00bfc731df60f44a7544eecc7dfaa7778",
          "message": "test(stdune): more copying tests (#7284)\n\ncover more error cases:\r\n- missing src\r\n- src is a directory\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-11T16:30:24-08:00",
          "tree_id": "faa0dc353332556f05e605cd937e0aec1af2655e",
          "url": "https://github.com/ocaml/dune/commit/f9a0e6f00bfc731df60f44a7544eecc7dfaa7778"
        },
        "date": 1678582497877,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.777771818613333",
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
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "64cf42dc58da7a6be862fc28e7721db7d9652788",
          "message": "Apply suggestions from code review\n\nCo-authored-by: Ali Caglayan <alizter@gmail.com>\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-12T01:56:22+01:00",
          "tree_id": "0327a7034a981c7453944c8bfac315ffec7ab19a",
          "url": "https://github.com/ocaml/dune/commit/64cf42dc58da7a6be862fc28e7721db7d9652788"
        },
        "date": 1678584052423,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "67.45445989602001",
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
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "64cf42dc58da7a6be862fc28e7721db7d9652788",
          "message": "Apply suggestions from code review\n\nCo-authored-by: Ali Caglayan <alizter@gmail.com>\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-12T01:56:22+01:00",
          "tree_id": "0327a7034a981c7453944c8bfac315ffec7ab19a",
          "url": "https://github.com/ocaml/dune/commit/64cf42dc58da7a6be862fc28e7721db7d9652788"
        },
        "date": 1678584063429,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.6483879716933334",
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
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "73ead70ca2e3d0a029f8adae0817502b1b0dae5e",
          "message": "fix(stdune): copy file error message on macos\n\nRemoe the additional \"error:\" prefix\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>\n\n<!-- ps-id: 759a4dea-1244-4591-ad38-cecd44adea5b -->",
          "timestamp": "2023-03-12T01:59:15+01:00",
          "tree_id": "51cb81c219c70fef6cf17a503c9f053622276838",
          "url": "https://github.com/ocaml/dune/commit/73ead70ca2e3d0a029f8adae0817502b1b0dae5e"
        },
        "date": 1678584198452,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "67.71211845110666",
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
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "73ead70ca2e3d0a029f8adae0817502b1b0dae5e",
          "message": "fix(stdune): copy file error message on macos\n\nRemoe the additional \"error:\" prefix\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>\n\n<!-- ps-id: 759a4dea-1244-4591-ad38-cecd44adea5b -->",
          "timestamp": "2023-03-12T01:59:15+01:00",
          "tree_id": "51cb81c219c70fef6cf17a503c9f053622276838",
          "url": "https://github.com/ocaml/dune/commit/73ead70ca2e3d0a029f8adae0817502b1b0dae5e"
        },
        "date": 1678584209237,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.7544657565400001",
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
          "id": "449cbd322ca2a2c50d437d2a2b69d81d5bdf0439",
          "message": "chore: improve file copying benchmark (#7294)\n\nallow changing the directory and the size of files\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-11T22:13:19-08:00",
          "tree_id": "5ce869c6ce3e4c036e9b8c727748a3bf3276d1d9",
          "url": "https://github.com/ocaml/dune/commit/449cbd322ca2a2c50d437d2a2b69d81d5bdf0439"
        },
        "date": 1678603116133,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "70.66028105646667",
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
          "id": "449cbd322ca2a2c50d437d2a2b69d81d5bdf0439",
          "message": "chore: improve file copying benchmark (#7294)\n\nallow changing the directory and the size of files\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-11T22:13:19-08:00",
          "tree_id": "5ce869c6ce3e4c036e9b8c727748a3bf3276d1d9",
          "url": "https://github.com/ocaml/dune/commit/449cbd322ca2a2c50d437d2a2b69d81d5bdf0439"
        },
        "date": 1678603127870,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.8578378103200002",
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
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "789264ff111a79e3a2d760bf1de55a286c4a0396",
          "message": "fix(nix): use `extraBuildInputs` in `mkSlim`\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-12T22:00:17+01:00",
          "tree_id": "350b3364a6cb94f78a52f4c78264a6a5eadb52f7",
          "url": "https://github.com/ocaml/dune/commit/789264ff111a79e3a2d760bf1de55a286c4a0396"
        },
        "date": 1678656247659,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "65.98503882848668",
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
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "789264ff111a79e3a2d760bf1de55a286c4a0396",
          "message": "fix(nix): use `extraBuildInputs` in `mkSlim`\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-12T22:00:17+01:00",
          "tree_id": "350b3364a6cb94f78a52f4c78264a6a5eadb52f7",
          "url": "https://github.com/ocaml/dune/commit/789264ff111a79e3a2d760bf1de55a286c4a0396"
        },
        "date": 1678656258127,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.5843485730066666",
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
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "e5a98ee12081cf3571e5a6d4d1aa14ddf392eb0d",
          "message": "fix(stdune): copy file error message consistency\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>\n\n<!-- ps-id: 97fe8b6e-0238-496c-b1d2-0683b775adb5 -->",
          "timestamp": "2023-03-12T22:11:15+01:00",
          "tree_id": "a3d2a4f33bcd0f6c19ea09b866740081575d7819",
          "url": "https://github.com/ocaml/dune/commit/e5a98ee12081cf3571e5a6d4d1aa14ddf392eb0d"
        },
        "date": 1678657204937,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "79.38522131396",
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
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "e5a98ee12081cf3571e5a6d4d1aa14ddf392eb0d",
          "message": "fix(stdune): copy file error message consistency\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>\n\n<!-- ps-id: 97fe8b6e-0238-496c-b1d2-0683b775adb5 -->",
          "timestamp": "2023-03-12T22:11:15+01:00",
          "tree_id": "a3d2a4f33bcd0f6c19ea09b866740081575d7819",
          "url": "https://github.com/ocaml/dune/commit/e5a98ee12081cf3571e5a6d4d1aa14ddf392eb0d"
        },
        "date": 1678657218497,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "2.081863411473333",
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
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "0c0d37eca9d47c0d19620a976cd57971188abc0c",
          "message": "Add repro for #7034\n\nSigned-off-by: Stephen Sherratt <stephen@sherra.tt>\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-13T01:13:03+01:00",
          "tree_id": "afc456c550342e2887e6da0c21b086f9f3148484",
          "url": "https://github.com/ocaml/dune/commit/0c0d37eca9d47c0d19620a976cd57971188abc0c"
        },
        "date": 1678667827381,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "66.10560768008668",
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
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "0c0d37eca9d47c0d19620a976cd57971188abc0c",
          "message": "Add repro for #7034\n\nSigned-off-by: Stephen Sherratt <stephen@sherra.tt>\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-13T01:13:03+01:00",
          "tree_id": "afc456c550342e2887e6da0c21b086f9f3148484",
          "url": "https://github.com/ocaml/dune/commit/0c0d37eca9d47c0d19620a976cd57971188abc0c"
        },
        "date": 1678667837938,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.5863990711133333",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "amokhov@janestreet.com",
            "name": "Andrey Mokhov",
            "username": "snowleopard"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "35e3958215e0fed860aecb24e977a46448950103",
          "message": "Add commutative monoids (#7240)\n\nSigned-off-by: Andrey Mokhov <andrey.mokhov@gmail.com>",
          "timestamp": "2023-03-13T15:13:27+08:00",
          "tree_id": "ab05c6b45656466be6764df730b089744059bc34",
          "url": "https://github.com/ocaml/dune/commit/35e3958215e0fed860aecb24e977a46448950103"
        },
        "date": 1678693123727,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "72.97250965325333",
            "unit": "seconds"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "amokhov@janestreet.com",
            "name": "Andrey Mokhov",
            "username": "snowleopard"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "35e3958215e0fed860aecb24e977a46448950103",
          "message": "Add commutative monoids (#7240)\n\nSigned-off-by: Andrey Mokhov <andrey.mokhov@gmail.com>",
          "timestamp": "2023-03-13T15:13:27+08:00",
          "tree_id": "ab05c6b45656466be6764df730b089744059bc34",
          "url": "https://github.com/ocaml/dune/commit/35e3958215e0fed860aecb24e977a46448950103"
        },
        "date": 1678693135063,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.8584442355933335",
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
          "id": "d7c0f37f8f64a5b3449aa09eae7c09b9ea2f5ea1",
          "message": "fix(melange): copy, don't symlink, runtime_deps (#7267)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-13T10:13:09-07:00",
          "tree_id": "54740154af335555581ca25f2017e0d4689f4a97",
          "url": "https://github.com/ocaml/dune/commit/d7c0f37f8f64a5b3449aa09eae7c09b9ea2f5ea1"
        },
        "date": 1678729137703,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "73.23522120425334",
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
          "id": "d7c0f37f8f64a5b3449aa09eae7c09b9ea2f5ea1",
          "message": "fix(melange): copy, don't symlink, runtime_deps (#7267)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-13T10:13:09-07:00",
          "tree_id": "54740154af335555581ca25f2017e0d4689f4a97",
          "url": "https://github.com/ocaml/dune/commit/d7c0f37f8f64a5b3449aa09eae7c09b9ea2f5ea1"
        },
        "date": 1678729149898,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.89932190312",
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
          "id": "988634c6c4a6a24c81fca523c59ff17a7467e193",
          "message": "fix(melange): rename `entries` to `modules` in `melange.emit` (#7302)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-13T10:17:41-07:00",
          "tree_id": "b388e03afc4f5bd16df2ef4cd544b05f5f7703fc",
          "url": "https://github.com/ocaml/dune/commit/988634c6c4a6a24c81fca523c59ff17a7467e193"
        },
        "date": 1678729423090,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "71.97670066327333",
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
          "id": "988634c6c4a6a24c81fca523c59ff17a7467e193",
          "message": "fix(melange): rename `entries` to `modules` in `melange.emit` (#7302)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-13T10:17:41-07:00",
          "tree_id": "b388e03afc4f5bd16df2ef4cd544b05f5f7703fc",
          "url": "https://github.com/ocaml/dune/commit/988634c6c4a6a24c81fca523c59ff17a7467e193"
        },
        "date": 1678729434954,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.8340346040733335",
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
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "b444ea9f36511286a7ddb9c3e9ffef16fb03536b",
          "message": "chore(nix): remove OCaml nix-overlays\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-13T20:01:46+01:00",
          "tree_id": "790071e21212acb32eefc52f8ef86c9aabedd0e9",
          "url": "https://github.com/ocaml/dune/commit/b444ea9f36511286a7ddb9c3e9ffef16fb03536b"
        },
        "date": 1678735547511,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "66.52432529885334",
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
            "email": "alizter@gmail.com",
            "name": "Ali Caglayan",
            "username": "Alizter"
          },
          "distinct": true,
          "id": "b444ea9f36511286a7ddb9c3e9ffef16fb03536b",
          "message": "chore(nix): remove OCaml nix-overlays\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-13T20:01:46+01:00",
          "tree_id": "790071e21212acb32eefc52f8ef86c9aabedd0e9",
          "url": "https://github.com/ocaml/dune/commit/b444ea9f36511286a7ddb9c3e9ffef16fb03536b"
        },
        "date": 1678735557692,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.6054225390466665",
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
          "id": "21914b91f66a94e2cae33b9b19ea1521b6104d8a",
          "message": "fix(rpc): ignore sigpipe on macos clients (#7299)\n\nRPC clients that suddenly disconnect will no longer raise sigpipe and\r\nmake dune terminate.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-13T12:26:10-07:00",
          "tree_id": "8912e6faccd7d85a04c9ad3400af1070e1cda50f",
          "url": "https://github.com/ocaml/dune/commit/21914b91f66a94e2cae33b9b19ea1521b6104d8a"
        },
        "date": 1678737105569,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "71.53247453947999",
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
          "id": "21914b91f66a94e2cae33b9b19ea1521b6104d8a",
          "message": "fix(rpc): ignore sigpipe on macos clients (#7299)\n\nRPC clients that suddenly disconnect will no longer raise sigpipe and\r\nmake dune terminate.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-13T12:26:10-07:00",
          "tree_id": "8912e6faccd7d85a04c9ad3400af1070e1cda50f",
          "url": "https://github.com/ocaml/dune/commit/21914b91f66a94e2cae33b9b19ea1521b6104d8a"
        },
        "date": 1678737117222,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.78266578318",
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
          "id": "66523e542511028a6cb72d80471f358aab30d2eb",
          "message": "feat(melange): add `melange.runtime_deps` field in library (#7234)\n\n* feat(melange): add `melange.runtime_deps` field in library\r\n\r\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-13T16:10:16-07:00",
          "tree_id": "c3c02da57a85c69afcc5e465890bb9d86f4c6127",
          "url": "https://github.com/ocaml/dune/commit/66523e542511028a6cb72d80471f358aab30d2eb"
        },
        "date": 1678751068846,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "66.54362627919333",
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
          "id": "66523e542511028a6cb72d80471f358aab30d2eb",
          "message": "feat(melange): add `melange.runtime_deps` field in library (#7234)\n\n* feat(melange): add `melange.runtime_deps` field in library\r\n\r\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-13T16:10:16-07:00",
          "tree_id": "c3c02da57a85c69afcc5e465890bb9d86f4c6127",
          "url": "https://github.com/ocaml/dune/commit/66523e542511028a6cb72d80471f358aab30d2eb"
        },
        "date": 1678751079673,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.60436508076",
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
          "id": "7e27809863060fe315bfeebef262990b46e89e87",
          "message": "refactor: rename pthread_chdir_stubs (#7310)\n\nit no longer just contains stubs for chdir\r\n\r\nalso take the opportunity to format it automatically\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-13T16:10:51-07:00",
          "tree_id": "02ff87c75f5d23539847e101abd284af5c3b3012",
          "url": "https://github.com/ocaml/dune/commit/7e27809863060fe315bfeebef262990b46e89e87"
        },
        "date": 1678751648617,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "85.81716073624666",
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
          "id": "7e27809863060fe315bfeebef262990b46e89e87",
          "message": "refactor: rename pthread_chdir_stubs (#7310)\n\nit no longer just contains stubs for chdir\r\n\r\nalso take the opportunity to format it automatically\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-13T16:10:51-07:00",
          "tree_id": "02ff87c75f5d23539847e101abd284af5c3b3012",
          "url": "https://github.com/ocaml/dune/commit/7e27809863060fe315bfeebef262990b46e89e87"
        },
        "date": 1678751663169,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "2.246957416866666",
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
          "id": "8621946251ef39e52c115cb36d79d5f56818dd04",
          "message": "fix(stdlib): pass flags when building stdlib.ml (#7241)\n\nReported by @gretay-js.\r\n\r\nThis ensures that when building `stdlib.ml` (the main module of a\r\nlibrary with `(stdlib)`), flags set in the corresponding stanza\r\n`(library)` are correctly passed.\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>",
          "timestamp": "2023-03-14T10:08:46+01:00",
          "tree_id": "f455d9414c9c8ae0c0e6767b07525663d9ca935d",
          "url": "https://github.com/ocaml/dune/commit/8621946251ef39e52c115cb36d79d5f56818dd04"
        },
        "date": 1678786403872,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "70.81605360412668",
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
          "id": "8621946251ef39e52c115cb36d79d5f56818dd04",
          "message": "fix(stdlib): pass flags when building stdlib.ml (#7241)\n\nReported by @gretay-js.\r\n\r\nThis ensures that when building `stdlib.ml` (the main module of a\r\nlibrary with `(stdlib)`), flags set in the corresponding stanza\r\n`(library)` are correctly passed.\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>",
          "timestamp": "2023-03-14T10:08:46+01:00",
          "tree_id": "f455d9414c9c8ae0c0e6767b07525663d9ca935d",
          "url": "https://github.com/ocaml/dune/commit/8621946251ef39e52c115cb36d79d5f56818dd04"
        },
        "date": 1678786415196,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.7896383579466668",
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
          "id": "9a36c025e228115963bc462d21ec948dff24b7a6",
          "message": "test: do not load init toplevel files (#7315)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-14T10:23:51-07:00",
          "tree_id": "4f004d014e22c9ec462a98ce5ec1ca2142d7c065",
          "url": "https://github.com/ocaml/dune/commit/9a36c025e228115963bc462d21ec948dff24b7a6"
        },
        "date": 1678816338014,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "76.74511843868666",
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
          "id": "9a36c025e228115963bc462d21ec948dff24b7a6",
          "message": "test: do not load init toplevel files (#7315)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-14T10:23:51-07:00",
          "tree_id": "4f004d014e22c9ec462a98ce5ec1ca2142d7c065",
          "url": "https://github.com/ocaml/dune/commit/9a36c025e228115963bc462d21ec948dff24b7a6"
        },
        "date": 1678816351356,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.9860931389399996",
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
          "id": "6374c6fe43b044771b26ad461efbe4ba975a6842",
          "message": "refactor(rpc): hide Session.create (#7317)\n\nIt was never used outside of the Csexp_rpc module\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-14T12:59:28-07:00",
          "tree_id": "68a92befe23eca28d4215cdb67abd21bfb67b639",
          "url": "https://github.com/ocaml/dune/commit/6374c6fe43b044771b26ad461efbe4ba975a6842"
        },
        "date": 1678825399376,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "65.76994447007333",
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
          "id": "6374c6fe43b044771b26ad461efbe4ba975a6842",
          "message": "refactor(rpc): hide Session.create (#7317)\n\nIt was never used outside of the Csexp_rpc module\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-14T12:59:28-07:00",
          "tree_id": "68a92befe23eca28d4215cdb67abd21bfb67b639",
          "url": "https://github.com/ocaml/dune/commit/6374c6fe43b044771b26ad461efbe4ba975a6842"
        },
        "date": 1678825409399,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.6080300050733334",
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
          "id": "00fdccc5bdc423e5437c5ece0cff0316d31669aa",
          "message": "test: move toplevel plugins to own dir (#7316)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-14T13:00:46-07:00",
          "tree_id": "9d473a3d041916135fc9f531b67a0dde2b8245ca",
          "url": "https://github.com/ocaml/dune/commit/00fdccc5bdc423e5437c5ece0cff0316d31669aa"
        },
        "date": 1678825516332,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "69.24054790391332",
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
          "id": "00fdccc5bdc423e5437c5ece0cff0316d31669aa",
          "message": "test: move toplevel plugins to own dir (#7316)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-14T13:00:46-07:00",
          "tree_id": "9d473a3d041916135fc9f531b67a0dde2b8245ca",
          "url": "https://github.com/ocaml/dune/commit/00fdccc5bdc423e5437c5ece0cff0316d31669aa"
        },
        "date": 1678825527805,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.7948334197466667",
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
          "id": "59cc644a039107e97d6d45dd8a046a26d15b8e22",
          "message": "test: odoc improvements (#7305)\n\nmove odoc test to odoc/ dir\r\nmove odoc test stanzas to odoc/ dir\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-14T13:35:26-07:00",
          "tree_id": "b444805a44904ecf08beb07a7163fdaf89a5ddc9",
          "url": "https://github.com/ocaml/dune/commit/59cc644a039107e97d6d45dd8a046a26d15b8e22"
        },
        "date": 1678827609347,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "67.79496804304665",
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
          "id": "59cc644a039107e97d6d45dd8a046a26d15b8e22",
          "message": "test: odoc improvements (#7305)\n\nmove odoc test to odoc/ dir\r\nmove odoc test stanzas to odoc/ dir\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-14T13:35:26-07:00",
          "tree_id": "b444805a44904ecf08beb07a7163fdaf89a5ddc9",
          "url": "https://github.com/ocaml/dune/commit/59cc644a039107e97d6d45dd8a046a26d15b8e22"
        },
        "date": 1678827621008,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.6174934220266668",
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
          "id": "aafa99289d7f570f4a998aaa88364f06c073180f",
          "message": "refactor(csexp_rpc): remove useless socket arg (#7318)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>\r\n\r\n<!-- ps-id: ec04f08e-5c43-4325-af38-6b32ae471217 -->",
          "timestamp": "2023-03-14T13:37:55-07:00",
          "tree_id": "8ee35a42bf1da96c17f0a1458ce3fd08fac9d3b9",
          "url": "https://github.com/ocaml/dune/commit/aafa99289d7f570f4a998aaa88364f06c073180f"
        },
        "date": 1678827756543,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "66.47716160528",
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
          "id": "aafa99289d7f570f4a998aaa88364f06c073180f",
          "message": "refactor(csexp_rpc): remove useless socket arg (#7318)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>\r\n\r\n<!-- ps-id: ec04f08e-5c43-4325-af38-6b32ae471217 -->",
          "timestamp": "2023-03-14T13:37:55-07:00",
          "tree_id": "8ee35a42bf1da96c17f0a1458ce3fd08fac9d3b9",
          "url": "https://github.com/ocaml/dune/commit/aafa99289d7f570f4a998aaa88364f06c073180f"
        },
        "date": 1678827767392,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.6117482512066665",
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
          "id": "846831c7cc2443db5f3aa0e12a43d9ba48cdd639",
          "message": "fix(ci): restrict jsoo version (#7324)\n\nOur test suite is incompatible with 5.1.0.\r\nThis ensures it is not picked by CI.\r\nThis bound is to be removed once a fixed version has been released.\r\n\r\nSee ocsigen/js_of_ocaml#1430\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>",
          "timestamp": "2023-03-15T16:24:37+01:00",
          "tree_id": "e7c95f691bdf8c30aeb622b813faff546141dd7d",
          "url": "https://github.com/ocaml/dune/commit/846831c7cc2443db5f3aa0e12a43d9ba48cdd639"
        },
        "date": 1678895596471,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "81.11108858141999",
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
          "id": "846831c7cc2443db5f3aa0e12a43d9ba48cdd639",
          "message": "fix(ci): restrict jsoo version (#7324)\n\nOur test suite is incompatible with 5.1.0.\r\nThis ensures it is not picked by CI.\r\nThis bound is to be removed once a fixed version has been released.\r\n\r\nSee ocsigen/js_of_ocaml#1430\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>",
          "timestamp": "2023-03-15T16:24:37+01:00",
          "tree_id": "e7c95f691bdf8c30aeb622b813faff546141dd7d",
          "url": "https://github.com/ocaml/dune/commit/846831c7cc2443db5f3aa0e12a43d9ba48cdd639"
        },
        "date": 1678895610038,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "2.0845746746133336",
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
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "distinct": true,
          "id": "ef9bc1df2213541bdd91170516a8031d9766c7ac",
          "message": "fix(melange): allow runtime_deps in the melange target dir\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-15T19:38:35-07:00",
          "tree_id": "cc1f1f039cd2dccac16444cd4593d192af40e3d9",
          "url": "https://github.com/ocaml/dune/commit/ef9bc1df2213541bdd91170516a8031d9766c7ac"
        },
        "date": 1678936087070,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "64.46080635225333",
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
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "distinct": true,
          "id": "ef9bc1df2213541bdd91170516a8031d9766c7ac",
          "message": "fix(melange): allow runtime_deps in the melange target dir\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-15T19:38:35-07:00",
          "tree_id": "cc1f1f039cd2dccac16444cd4593d192af40e3d9",
          "url": "https://github.com/ocaml/dune/commit/ef9bc1df2213541bdd91170516a8031d9766c7ac"
        },
        "date": 1678936097001,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.5823222636066667",
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
          "id": "745dca81e0adc6deab9e4bfaed33e985c80641dd",
          "message": "chore(nix): upgrade melange that picks up `entries` -> `modules` rename (#7320)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-15T19:47:47-07:00",
          "tree_id": "75416581c02cf21f49986b5f66c8b2eb4cd451c7",
          "url": "https://github.com/ocaml/dune/commit/745dca81e0adc6deab9e4bfaed33e985c80641dd"
        },
        "date": 1678936682056,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "71.32070943852666",
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
          "id": "745dca81e0adc6deab9e4bfaed33e985c80641dd",
          "message": "chore(nix): upgrade melange that picks up `entries` -> `modules` rename (#7320)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-15T19:47:47-07:00",
          "tree_id": "75416581c02cf21f49986b5f66c8b2eb4cd451c7",
          "url": "https://github.com/ocaml/dune/commit/745dca81e0adc6deab9e4bfaed33e985c80641dd"
        },
        "date": 1678936693465,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.8420165493066667",
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
          "id": "be1187e1306370a2d77cd8f79ce6c4e2baa13a7d",
          "message": "refactor(melange): propagate runtime_deps `loc` in Melange_rules (#7331)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-15T20:35:23-07:00",
          "tree_id": "7b15271f2ae8ac3437f0363f3d04762ef68395bc",
          "url": "https://github.com/ocaml/dune/commit/be1187e1306370a2d77cd8f79ce6c4e2baa13a7d"
        },
        "date": 1678939620497,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "89.90010959041332",
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
          "id": "be1187e1306370a2d77cd8f79ce6c4e2baa13a7d",
          "message": "refactor(melange): propagate runtime_deps `loc` in Melange_rules (#7331)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-15T20:35:23-07:00",
          "tree_id": "7b15271f2ae8ac3437f0363f3d04762ef68395bc",
          "url": "https://github.com/ocaml/dune/commit/be1187e1306370a2d77cd8f79ce6c4e2baa13a7d"
        },
        "date": 1678939635507,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "2.4215192015866664",
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
          "id": "20ad5e109e3879a54e988fd4f9b2922c7a6d5fb9",
          "message": "fix(rules): merge allowed subdirs correctly for Redirect_to_parent rules (#7207)\n\n* fix(rules): merge allowed subdirs correctly for Redirect_to_parent rules",
          "timestamp": "2023-03-16T00:18:54-07:00",
          "tree_id": "99e07b91df680686e63c2b9e76e077def14e25b3",
          "url": "https://github.com/ocaml/dune/commit/20ad5e109e3879a54e988fd4f9b2922c7a6d5fb9"
        },
        "date": 1678952849861,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "77.94778774714",
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
          "id": "20ad5e109e3879a54e988fd4f9b2922c7a6d5fb9",
          "message": "fix(rules): merge allowed subdirs correctly for Redirect_to_parent rules (#7207)\n\n* fix(rules): merge allowed subdirs correctly for Redirect_to_parent rules",
          "timestamp": "2023-03-16T00:18:54-07:00",
          "tree_id": "99e07b91df680686e63c2b9e76e077def14e25b3",
          "url": "https://github.com/ocaml/dune/commit/20ad5e109e3879a54e988fd4f9b2922c7a6d5fb9"
        },
        "date": 1678952863803,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "2.16153589754",
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
          "id": "a29a12950f8d8f97ecc84fe3f70d28144111075b",
          "message": "test(melange): switch to sh (#7332)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-16T00:19:23-07:00",
          "tree_id": "e19141c8b1c3b4f7a234eac38f283c47787ec7c0",
          "url": "https://github.com/ocaml/dune/commit/a29a12950f8d8f97ecc84fe3f70d28144111075b"
        },
        "date": 1678952961263,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "83.95280304216",
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
          "id": "a29a12950f8d8f97ecc84fe3f70d28144111075b",
          "message": "test(melange): switch to sh (#7332)\n\nSigned-off-by: Antonio Nuno Monteiro <anmonteiro@gmail.com>",
          "timestamp": "2023-03-16T00:19:23-07:00",
          "tree_id": "e19141c8b1c3b4f7a234eac38f283c47787ec7c0",
          "url": "https://github.com/ocaml/dune/commit/a29a12950f8d8f97ecc84fe3f70d28144111075b"
        },
        "date": 1678952975865,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "2.27649273624",
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
          "id": "c0c0acba6b4d0744198e0ce06dae0095ff0c6384",
          "message": "Add monorepo benchmark (#7202)\n\n* Add monorepo benchmark\r\n\r\nThis adds a current-bench benchmark that measures the time it takes to\r\nbuild a large monorepo composed from opam packages.\r\n\r\nSigned-off-by: Stephen Sherratt <stephen@sherra.tt>",
          "timestamp": "2023-03-16T08:30:40-07:00",
          "tree_id": "8c5741ec18ba2fe104c2774da6863ad1b8a68e0c",
          "url": "https://github.com/ocaml/dune/commit/c0c0acba6b4d0744198e0ce06dae0095ff0c6384"
        },
        "date": 1678982207217,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "69.58832462830668",
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
          "id": "c0c0acba6b4d0744198e0ce06dae0095ff0c6384",
          "message": "Add monorepo benchmark (#7202)\n\n* Add monorepo benchmark\r\n\r\nThis adds a current-bench benchmark that measures the time it takes to\r\nbuild a large monorepo composed from opam packages.\r\n\r\nSigned-off-by: Stephen Sherratt <stephen@sherra.tt>",
          "timestamp": "2023-03-16T08:30:40-07:00",
          "tree_id": "8c5741ec18ba2fe104c2774da6863ad1b8a68e0c",
          "url": "https://github.com/ocaml/dune/commit/c0c0acba6b4d0744198e0ce06dae0095ff0c6384"
        },
        "date": 1678982218376,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.71294539346",
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
          "id": "649031728346482a83441c26c0bbe06ecd89cce2",
          "message": "Update opam-file-format to 2.1.5 (#7328)\n\n* Update opam-file-format to 2.1.5\r\n\r\nSigned-off-by: Marek Kubica <marek@tarides.com>",
          "timestamp": "2023-03-16T09:22:38-07:00",
          "tree_id": "a4abe01551c0de278b22657cfa623b0897ec74ec",
          "url": "https://github.com/ocaml/dune/commit/649031728346482a83441c26c0bbe06ecd89cce2"
        },
        "date": 1678985214227,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "65.34538010081333",
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
          "id": "649031728346482a83441c26c0bbe06ecd89cce2",
          "message": "Update opam-file-format to 2.1.5 (#7328)\n\n* Update opam-file-format to 2.1.5\r\n\r\nSigned-off-by: Marek Kubica <marek@tarides.com>",
          "timestamp": "2023-03-16T09:22:38-07:00",
          "tree_id": "a4abe01551c0de278b22657cfa623b0897ec74ec",
          "url": "https://github.com/ocaml/dune/commit/649031728346482a83441c26c0bbe06ecd89cce2"
        },
        "date": 1678985224092,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.5782005852466667",
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
          "id": "22eab0558fd06cdb0c3ba7ae8cf0074fb7c17a55",
          "message": "refactor: rename dune_config (#7333)\n\nRename dune_config to dune_config_file. We can still access the old\r\nnames by opening Dune_config.\r\n\r\nIn preparation for introducing a dune_config library that will introduce\r\na [Dune_config.Config] module.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-16T19:25:43-07:00",
          "tree_id": "c56d6df09a65cf0d012a8e35a3d13848e14b3a3a",
          "url": "https://github.com/ocaml/dune/commit/22eab0558fd06cdb0c3ba7ae8cf0074fb7c17a55"
        },
        "date": 1679021668466,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "77.22123281640665",
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
          "id": "22eab0558fd06cdb0c3ba7ae8cf0074fb7c17a55",
          "message": "refactor: rename dune_config (#7333)\n\nRename dune_config to dune_config_file. We can still access the old\r\nnames by opening Dune_config.\r\n\r\nIn preparation for introducing a dune_config library that will introduce\r\na [Dune_config.Config] module.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-16T19:25:43-07:00",
          "tree_id": "c56d6df09a65cf0d012a8e35a3d13848e14b3a3a",
          "url": "https://github.com/ocaml/dune/commit/22eab0558fd06cdb0c3ba7ae8cf0074fb7c17a55"
        },
        "date": 1679021681389,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "2.0060696096000004",
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
          "id": "6ac9daba4d18b7fe52a2026881279dc3a6680583",
          "message": "benchmarks: add synthetic watch (#7255)\n\n* benchmarks: add synthetic watch\r\n\r\nSigned-off-by: Javier Chávarri <javier.chavarri@gmail.com>",
          "timestamp": "2023-03-16T19:28:14-07:00",
          "tree_id": "9f89b7728daca543771684882ade802631ee6751",
          "url": "https://github.com/ocaml/dune/commit/6ac9daba4d18b7fe52a2026881279dc3a6680583"
        },
        "date": 1679021777823,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "70.62186479266667",
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
          "id": "6ac9daba4d18b7fe52a2026881279dc3a6680583",
          "message": "benchmarks: add synthetic watch (#7255)\n\n* benchmarks: add synthetic watch\r\n\r\nSigned-off-by: Javier Chávarri <javier.chavarri@gmail.com>",
          "timestamp": "2023-03-16T19:28:14-07:00",
          "tree_id": "9f89b7728daca543771684882ade802631ee6751",
          "url": "https://github.com/ocaml/dune/commit/6ac9daba4d18b7fe52a2026881279dc3a6680583"
        },
        "date": 1679021788928,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.7927148667933335",
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
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "557e5089782b2441a5b4dd9198d4ebff11299d85",
          "message": "raise error when using describe pp command with staged_pps (#7167)\n\nSigned-off-by: Corentin Leruth <corentin.leruth@gmail.com>",
          "timestamp": "2023-03-16T19:41:51-07:00",
          "tree_id": "47f95224d1846aa25a8f83db8e2139d5fbb7ab89",
          "url": "https://github.com/ocaml/dune/commit/557e5089782b2441a5b4dd9198d4ebff11299d85"
        },
        "date": 1679022613813,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "70.11023765190667",
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
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "557e5089782b2441a5b4dd9198d4ebff11299d85",
          "message": "raise error when using describe pp command with staged_pps (#7167)\n\nSigned-off-by: Corentin Leruth <corentin.leruth@gmail.com>",
          "timestamp": "2023-03-16T19:41:51-07:00",
          "tree_id": "47f95224d1846aa25a8f83db8e2139d5fbb7ab89",
          "url": "https://github.com/ocaml/dune/commit/557e5089782b2441a5b4dd9198d4ebff11299d85"
        },
        "date": 1679022624988,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.8260141944400001",
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
          "id": "695a4506a6f0a72023a509f8ef78734b17b5f23a",
          "message": "Revert \"fix(ci): restrict jsoo version\" (#7342)\n\nThis reverts commit 846831c7cc2443db5f3aa0e12a43d9ba48cdd639.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-17T08:51:25-07:00",
          "tree_id": "ae7ea767488f2005175054106784e24b04ecf11f",
          "url": "https://github.com/ocaml/dune/commit/695a4506a6f0a72023a509f8ef78734b17b5f23a"
        },
        "date": 1679069979319,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "68.37261449159332",
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
          "id": "695a4506a6f0a72023a509f8ef78734b17b5f23a",
          "message": "Revert \"fix(ci): restrict jsoo version\" (#7342)\n\nThis reverts commit 846831c7cc2443db5f3aa0e12a43d9ba48cdd639.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-17T08:51:25-07:00",
          "tree_id": "ae7ea767488f2005175054106784e24b04ecf11f",
          "url": "https://github.com/ocaml/dune/commit/695a4506a6f0a72023a509f8ef78734b17b5f23a"
        },
        "date": 1679069989970,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.6307112886933333",
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
          "id": "8cdeba0ed881fdec2b369d584012d4f95242e323",
          "message": "fix(rpc): ignore sigpipe on linux (#7319)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-17T09:09:07-07:00",
          "tree_id": "9c21adffaadd084b20dc2d3d6965317f55dc9ceb",
          "url": "https://github.com/ocaml/dune/commit/8cdeba0ed881fdec2b369d584012d4f95242e323"
        },
        "date": 1679071003526,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "63.48074009952666",
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
          "id": "8cdeba0ed881fdec2b369d584012d4f95242e323",
          "message": "fix(rpc): ignore sigpipe on linux (#7319)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-17T09:09:07-07:00",
          "tree_id": "9c21adffaadd084b20dc2d3d6965317f55dc9ceb",
          "url": "https://github.com/ocaml/dune/commit/8cdeba0ed881fdec2b369d584012d4f95242e323"
        },
        "date": 1679071013726,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.5280210829399998",
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
          "id": "72d8172b408f97cc6efebd28eb348c2fa0dee879",
          "message": "refactor(engine): remove source tree traversals (#7150)\n\nPreviously, the engine would know about directories for computing\r\nsource_tree, and recursive aliases.\r\n\r\nWith this PR, these operations are generalized to work over arbitrary\r\ndirectory traversals.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-17T09:39:49-07:00",
          "tree_id": "63631fddb3edcc00aa9c8e89e7f07927bb942e82",
          "url": "https://github.com/ocaml/dune/commit/72d8172b408f97cc6efebd28eb348c2fa0dee879"
        },
        "date": 1679073302441,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "83.95263312977333",
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
          "id": "72d8172b408f97cc6efebd28eb348c2fa0dee879",
          "message": "refactor(engine): remove source tree traversals (#7150)\n\nPreviously, the engine would know about directories for computing\r\nsource_tree, and recursive aliases.\r\n\r\nWith this PR, these operations are generalized to work over arbitrary\r\ndirectory traversals.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-17T09:39:49-07:00",
          "tree_id": "63631fddb3edcc00aa9c8e89e7f07927bb942e82",
          "url": "https://github.com/ocaml/dune/commit/72d8172b408f97cc6efebd28eb348c2fa0dee879"
        },
        "date": 1679073317026,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "2.230701481926667",
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
          "id": "dfd5d29fffe5a1e85688ffb3941022983eae7525",
          "message": "Update doc/hacking.rst\n\nCo-authored-by: Andrey Mokhov <andrey.mokhov@gmail.com>\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-17T18:23:18+01:00",
          "tree_id": "9054e5f6d0a99a3a53726a54dc1dec1988e29e03",
          "url": "https://github.com/ocaml/dune/commit/dfd5d29fffe5a1e85688ffb3941022983eae7525"
        },
        "date": 1679075665721,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "75.09500779689334",
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
          "id": "dfd5d29fffe5a1e85688ffb3941022983eae7525",
          "message": "Update doc/hacking.rst\n\nCo-authored-by: Andrey Mokhov <andrey.mokhov@gmail.com>\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-17T18:23:18+01:00",
          "tree_id": "9054e5f6d0a99a3a53726a54dc1dec1988e29e03",
          "url": "https://github.com/ocaml/dune/commit/dfd5d29fffe5a1e85688ffb3941022983eae7525"
        },
        "date": 1679075678312,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.9239014194600002",
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
          "id": "8b031d58633e5c9bac0a9fbced9fa27d719bb305",
          "message": "test(coq): add coq_scrub_args.sh script\n\nAdded a script `coq_scrub_args.sh` which will scrub the output of\ncommon command printing. This is useful for checking that the correct\nflags are being passed and saves us from having to mess around with\nsed everytime we wish to test something like this.\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-17T18:19:28+01:00",
          "tree_id": "d3c08dfc46410c191e138121bdb1d4fc0d875e8a",
          "url": "https://github.com/ocaml/dune/commit/8b031d58633e5c9bac0a9fbced9fa27d719bb305"
        },
        "date": 1679075774371,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "84.93913481415333",
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
          "id": "8b031d58633e5c9bac0a9fbced9fa27d719bb305",
          "message": "test(coq): add coq_scrub_args.sh script\n\nAdded a script `coq_scrub_args.sh` which will scrub the output of\ncommon command printing. This is useful for checking that the correct\nflags are being passed and saves us from having to mess around with\nsed everytime we wish to test something like this.\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-17T18:19:28+01:00",
          "tree_id": "d3c08dfc46410c191e138121bdb1d4fc0d875e8a",
          "url": "https://github.com/ocaml/dune/commit/8b031d58633e5c9bac0a9fbced9fa27d719bb305"
        },
        "date": 1679075788467,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "2.1921405141066668",
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
          "id": "8bea7ea00018f58112b3b8a68faa73cb87552412",
          "message": "feature: add a thread pool (#7201)\n\nreplace the current implementation with an unbounded number of workers\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-17T10:46:10-07:00",
          "tree_id": "cf2d1d281f98f277bc144c69678fd2e17e070c70",
          "url": "https://github.com/ocaml/dune/commit/8bea7ea00018f58112b3b8a68faa73cb87552412"
        },
        "date": 1679076826449,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "65.73692781416666",
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
          "id": "8bea7ea00018f58112b3b8a68faa73cb87552412",
          "message": "feature: add a thread pool (#7201)\n\nreplace the current implementation with an unbounded number of workers\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-17T10:46:10-07:00",
          "tree_id": "cf2d1d281f98f277bc144c69678fd2e17e070c70",
          "url": "https://github.com/ocaml/dune/commit/8bea7ea00018f58112b3b8a68faa73cb87552412"
        },
        "date": 1679076836342,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.5874349934599998",
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
          "id": "4245029f2914bcfdca915e9916a5155a0afa95fb",
          "message": "feature: add action runners (#6737)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-17T16:47:22-07:00",
          "tree_id": "c854afadcc9812714d9415eca4c789aeb33a416e",
          "url": "https://github.com/ocaml/dune/commit/4245029f2914bcfdca915e9916a5155a0afa95fb"
        },
        "date": 1679098511639,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "70.24727995705335",
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
          "id": "4245029f2914bcfdca915e9916a5155a0afa95fb",
          "message": "feature: add action runners (#6737)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-17T16:47:22-07:00",
          "tree_id": "c854afadcc9812714d9415eca4c789aeb33a416e",
          "url": "https://github.com/ocaml/dune/commit/4245029f2914bcfdca915e9916a5155a0afa95fb"
        },
        "date": 1679098523450,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.8079790909866666",
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
          "id": "c4fc52b3f1e68620d3a9712d46d4f710f0c09fb7",
          "message": "Doc: fix default location of cache on Windows (#7356)\n\nSigned-off-by: Nicolás Ojeda Bär <n.oje.bar@gmail.com>",
          "timestamp": "2023-03-20T15:06:35+01:00",
          "tree_id": "54b9c616aa715efd1b625d36735db5ac4280079d",
          "url": "https://github.com/ocaml/dune/commit/c4fc52b3f1e68620d3a9712d46d4f710f0c09fb7"
        },
        "date": 1679322846743,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "65.65450599547333",
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
          "id": "c4fc52b3f1e68620d3a9712d46d4f710f0c09fb7",
          "message": "Doc: fix default location of cache on Windows (#7356)\n\nSigned-off-by: Nicolás Ojeda Bär <n.oje.bar@gmail.com>",
          "timestamp": "2023-03-20T15:06:35+01:00",
          "tree_id": "54b9c616aa715efd1b625d36735db5ac4280079d",
          "url": "https://github.com/ocaml/dune/commit/c4fc52b3f1e68620d3a9712d46d4f710f0c09fb7"
        },
        "date": 1679322857777,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.5921532660266664",
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
          "id": "cc174110c2dcab83b0068825870e1484cecef464",
          "message": "Doc: split \"General concepts\" into several pages (#7232)\n\nAs discussed with @Alizter, this splits our giant \"General concepts\" page into\r\nsmaller ones.\r\n\r\nIn addition, this transforms references to the various sections into :doc:\r\nreferences to the documents themselves.\r\n\r\nThe content is mostly untouched, except:\r\n\r\n\"see the X section\" -> \"see X\" (now that it's not a section anymore)\r\nthere was a mixup between the \"sandboxing\" and \"dune action plugin\" references\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>\r\nCo-authored-by: Christine Rose <christinerose@users.noreply.github.com>",
          "timestamp": "2023-03-20T17:21:42+01:00",
          "tree_id": "872414ea39310a1aa2495271c0ed34998d5d9223",
          "url": "https://github.com/ocaml/dune/commit/cc174110c2dcab83b0068825870e1484cecef464"
        },
        "date": 1679330993979,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "66.92870879952",
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
          "id": "cc174110c2dcab83b0068825870e1484cecef464",
          "message": "Doc: split \"General concepts\" into several pages (#7232)\n\nAs discussed with @Alizter, this splits our giant \"General concepts\" page into\r\nsmaller ones.\r\n\r\nIn addition, this transforms references to the various sections into :doc:\r\nreferences to the documents themselves.\r\n\r\nThe content is mostly untouched, except:\r\n\r\n\"see the X section\" -> \"see X\" (now that it's not a section anymore)\r\nthere was a mixup between the \"sandboxing\" and \"dune action plugin\" references\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>\r\nCo-authored-by: Christine Rose <christinerose@users.noreply.github.com>",
          "timestamp": "2023-03-20T17:21:42+01:00",
          "tree_id": "872414ea39310a1aa2495271c0ed34998d5d9223",
          "url": "https://github.com/ocaml/dune/commit/cc174110c2dcab83b0068825870e1484cecef464"
        },
        "date": 1679331005902,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.6140798367",
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
          "id": "247664d098d6fe8e464f46169f9bf30006581d7b",
          "message": "Run inline tests in parallel (#7012)\n\n* feature(inline-tests): partitions\r\n\r\nAllow specifying partitions of inline tests. Partitions in the same test\r\nsuite will be executed concurrently.\r\n\r\nSigned-off-by: Hugo Heuzard <hugo.heuzard@gmail.com>",
          "timestamp": "2023-03-20T12:24:58-07:00",
          "tree_id": "6c4134e0c5f5f1efc83fdc6008ad079f40e633c0",
          "url": "https://github.com/ocaml/dune/commit/247664d098d6fe8e464f46169f9bf30006581d7b"
        },
        "date": 1679341937965,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "65.74545400711999",
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
          "id": "247664d098d6fe8e464f46169f9bf30006581d7b",
          "message": "Run inline tests in parallel (#7012)\n\n* feature(inline-tests): partitions\r\n\r\nAllow specifying partitions of inline tests. Partitions in the same test\r\nsuite will be executed concurrently.\r\n\r\nSigned-off-by: Hugo Heuzard <hugo.heuzard@gmail.com>",
          "timestamp": "2023-03-20T12:24:58-07:00",
          "tree_id": "6c4134e0c5f5f1efc83fdc6008ad079f40e633c0",
          "url": "https://github.com/ocaml/dune/commit/247664d098d6fe8e464f46169f9bf30006581d7b"
        },
        "date": 1679341948428,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.5545289312266668",
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
          "id": "b1e0bbb3ce44f3df569d7669d946b735939b39c0",
          "message": "dune coq top: fix capitialization in error messages\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-21T01:00:39+01:00",
          "tree_id": "047518031c07995f0ae2263bea79dcb3c4fe38e5",
          "url": "https://github.com/ocaml/dune/commit/b1e0bbb3ce44f3df569d7669d946b735939b39c0"
        },
        "date": 1679358879205,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "70.03404510176",
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
          "id": "b1e0bbb3ce44f3df569d7669d946b735939b39c0",
          "message": "dune coq top: fix capitialization in error messages\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-21T01:00:39+01:00",
          "tree_id": "047518031c07995f0ae2263bea79dcb3c4fe38e5",
          "url": "https://github.com/ocaml/dune/commit/b1e0bbb3ce44f3df569d7669d946b735939b39c0"
        },
        "date": 1679358890610,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.7997463670200002",
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
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "distinct": true,
          "id": "d920ff4e9bb0cab87b50ba968383ecc6a50e6c95",
          "message": "test(rpc): add test with server in eager mode\n\nSigned-off-by: Javier Chávarri <javier.chavarri@gmail.com>",
          "timestamp": "2023-03-20T17:36:34-07:00",
          "tree_id": "d8228291c6dbbf85af549247bff950e8343f7fd7",
          "url": "https://github.com/ocaml/dune/commit/d920ff4e9bb0cab87b50ba968383ecc6a50e6c95"
        },
        "date": 1679360594806,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (cold, Linux)",
            "value": "64.03653291688",
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
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "distinct": true,
          "id": "d920ff4e9bb0cab87b50ba968383ecc6a50e6c95",
          "message": "test(rpc): add test with server in eager mode\n\nSigned-off-by: Javier Chávarri <javier.chavarri@gmail.com>",
          "timestamp": "2023-03-20T17:36:34-07:00",
          "tree_id": "d8228291c6dbbf85af549247bff950e8343f7fd7",
          "url": "https://github.com/ocaml/dune/commit/d920ff4e9bb0cab87b50ba968383ecc6a50e6c95"
        },
        "date": 1679360604795,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic build time (warm, Linux)",
            "value": "1.5905657915066669",
            "unit": "seconds"
          }
        ]
      }
    ],
    "Synthetic Watch Benchmark": [
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
          "id": "6ac9daba4d18b7fe52a2026881279dc3a6680583",
          "message": "benchmarks: add synthetic watch (#7255)\n\n* benchmarks: add synthetic watch\r\n\r\nSigned-off-by: Javier Chávarri <javier.chavarri@gmail.com>",
          "timestamp": "2023-03-16T19:28:14-07:00",
          "tree_id": "9f89b7728daca543771684882ade802631ee6751",
          "url": "https://github.com/ocaml/dune/commit/6ac9daba4d18b7fe52a2026881279dc3a6680583"
        },
        "date": 1679020930871,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic watch build time (warm, Linux)",
            "value": "34.5657431764",
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
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "557e5089782b2441a5b4dd9198d4ebff11299d85",
          "message": "raise error when using describe pp command with staged_pps (#7167)\n\nSigned-off-by: Corentin Leruth <corentin.leruth@gmail.com>",
          "timestamp": "2023-03-16T19:41:51-07:00",
          "tree_id": "47f95224d1846aa25a8f83db8e2139d5fbb7ab89",
          "url": "https://github.com/ocaml/dune/commit/557e5089782b2441a5b4dd9198d4ebff11299d85"
        },
        "date": 1679021764019,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic watch build time (warm, Linux)",
            "value": "34.162460875006666",
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
          "id": "695a4506a6f0a72023a509f8ef78734b17b5f23a",
          "message": "Revert \"fix(ci): restrict jsoo version\" (#7342)\n\nThis reverts commit 846831c7cc2443db5f3aa0e12a43d9ba48cdd639.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-17T08:51:25-07:00",
          "tree_id": "ae7ea767488f2005175054106784e24b04ecf11f",
          "url": "https://github.com/ocaml/dune/commit/695a4506a6f0a72023a509f8ef78734b17b5f23a"
        },
        "date": 1679069126093,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic watch build time (warm, Linux)",
            "value": "33.66498990445333",
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
          "id": "8cdeba0ed881fdec2b369d584012d4f95242e323",
          "message": "fix(rpc): ignore sigpipe on linux (#7319)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-17T09:09:07-07:00",
          "tree_id": "9c21adffaadd084b20dc2d3d6965317f55dc9ceb",
          "url": "https://github.com/ocaml/dune/commit/8cdeba0ed881fdec2b369d584012d4f95242e323"
        },
        "date": 1679070198869,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic watch build time (warm, Linux)",
            "value": "30.590289170566667",
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
          "id": "72d8172b408f97cc6efebd28eb348c2fa0dee879",
          "message": "refactor(engine): remove source tree traversals (#7150)\n\nPreviously, the engine would know about directories for computing\r\nsource_tree, and recursive aliases.\r\n\r\nWith this PR, these operations are generalized to work over arbitrary\r\ndirectory traversals.\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-17T09:39:49-07:00",
          "tree_id": "63631fddb3edcc00aa9c8e89e7f07927bb942e82",
          "url": "https://github.com/ocaml/dune/commit/72d8172b408f97cc6efebd28eb348c2fa0dee879"
        },
        "date": 1679072255785,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic watch build time (warm, Linux)",
            "value": "41.06113519317333",
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
          "id": "8b031d58633e5c9bac0a9fbced9fa27d719bb305",
          "message": "test(coq): add coq_scrub_args.sh script\n\nAdded a script `coq_scrub_args.sh` which will scrub the output of\ncommon command printing. This is useful for checking that the correct\nflags are being passed and saves us from having to mess around with\nsed everytime we wish to test something like this.\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-17T18:19:28+01:00",
          "tree_id": "d3c08dfc46410c191e138121bdb1d4fc0d875e8a",
          "url": "https://github.com/ocaml/dune/commit/8b031d58633e5c9bac0a9fbced9fa27d719bb305"
        },
        "date": 1679074722101,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic watch build time (warm, Linux)",
            "value": "41.570630739846656",
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
          "id": "dfd5d29fffe5a1e85688ffb3941022983eae7525",
          "message": "Update doc/hacking.rst\n\nCo-authored-by: Andrey Mokhov <andrey.mokhov@gmail.com>\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-17T18:23:18+01:00",
          "tree_id": "9054e5f6d0a99a3a53726a54dc1dec1988e29e03",
          "url": "https://github.com/ocaml/dune/commit/dfd5d29fffe5a1e85688ffb3941022983eae7525"
        },
        "date": 1679074741453,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic watch build time (warm, Linux)",
            "value": "37.64113103032667",
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
          "id": "8bea7ea00018f58112b3b8a68faa73cb87552412",
          "message": "feature: add a thread pool (#7201)\n\nreplace the current implementation with an unbounded number of workers\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-17T10:46:10-07:00",
          "tree_id": "cf2d1d281f98f277bc144c69678fd2e17e070c70",
          "url": "https://github.com/ocaml/dune/commit/8bea7ea00018f58112b3b8a68faa73cb87552412"
        },
        "date": 1679075996694,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic watch build time (warm, Linux)",
            "value": "32.343031744546664",
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
          "id": "4245029f2914bcfdca915e9916a5155a0afa95fb",
          "message": "feature: add action runners (#6737)\n\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-17T16:47:22-07:00",
          "tree_id": "c854afadcc9812714d9415eca4c789aeb33a416e",
          "url": "https://github.com/ocaml/dune/commit/4245029f2914bcfdca915e9916a5155a0afa95fb"
        },
        "date": 1679097660244,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic watch build time (warm, Linux)",
            "value": "32.856120497519996",
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
          "id": "c4fc52b3f1e68620d3a9712d46d4f710f0c09fb7",
          "message": "Doc: fix default location of cache on Windows (#7356)\n\nSigned-off-by: Nicolás Ojeda Bär <n.oje.bar@gmail.com>",
          "timestamp": "2023-03-20T15:06:35+01:00",
          "tree_id": "54b9c616aa715efd1b625d36735db5ac4280079d",
          "url": "https://github.com/ocaml/dune/commit/c4fc52b3f1e68620d3a9712d46d4f710f0c09fb7"
        },
        "date": 1679322009808,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic watch build time (warm, Linux)",
            "value": "32.23289521992667",
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
          "id": "cc174110c2dcab83b0068825870e1484cecef464",
          "message": "Doc: split \"General concepts\" into several pages (#7232)\n\nAs discussed with @Alizter, this splits our giant \"General concepts\" page into\r\nsmaller ones.\r\n\r\nIn addition, this transforms references to the various sections into :doc:\r\nreferences to the documents themselves.\r\n\r\nThe content is mostly untouched, except:\r\n\r\n\"see the X section\" -> \"see X\" (now that it's not a section anymore)\r\nthere was a mixup between the \"sandboxing\" and \"dune action plugin\" references\r\n\r\nSigned-off-by: Etienne Millon <me@emillon.org>\r\nCo-authored-by: Christine Rose <christinerose@users.noreply.github.com>",
          "timestamp": "2023-03-20T17:21:42+01:00",
          "tree_id": "872414ea39310a1aa2495271c0ed34998d5d9223",
          "url": "https://github.com/ocaml/dune/commit/cc174110c2dcab83b0068825870e1484cecef464"
        },
        "date": 1679330139793,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic watch build time (warm, Linux)",
            "value": "33.378454295039994",
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
          "id": "247664d098d6fe8e464f46169f9bf30006581d7b",
          "message": "Run inline tests in parallel (#7012)\n\n* feature(inline-tests): partitions\r\n\r\nAllow specifying partitions of inline tests. Partitions in the same test\r\nsuite will be executed concurrently.\r\n\r\nSigned-off-by: Hugo Heuzard <hugo.heuzard@gmail.com>",
          "timestamp": "2023-03-20T12:24:58-07:00",
          "tree_id": "6c4134e0c5f5f1efc83fdc6008ad079f40e633c0",
          "url": "https://github.com/ocaml/dune/commit/247664d098d6fe8e464f46169f9bf30006581d7b"
        },
        "date": 1679341110019,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic watch build time (warm, Linux)",
            "value": "32.33395366055333",
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
          "id": "b1e0bbb3ce44f3df569d7669d946b735939b39c0",
          "message": "dune coq top: fix capitialization in error messages\n\nSigned-off-by: Ali Caglayan <alizter@gmail.com>",
          "timestamp": "2023-03-21T01:00:39+01:00",
          "tree_id": "047518031c07995f0ae2263bea79dcb3c4fe38e5",
          "url": "https://github.com/ocaml/dune/commit/b1e0bbb3ce44f3df569d7669d946b735939b39c0"
        },
        "date": 1679358016643,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic watch build time (warm, Linux)",
            "value": "34.551392822593336",
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
            "email": "me@rgrinberg.com",
            "name": "Rudi Grinberg",
            "username": "rgrinberg"
          },
          "distinct": true,
          "id": "d920ff4e9bb0cab87b50ba968383ecc6a50e6c95",
          "message": "test(rpc): add test with server in eager mode\n\nSigned-off-by: Javier Chávarri <javier.chavarri@gmail.com>",
          "timestamp": "2023-03-20T17:36:34-07:00",
          "tree_id": "d8228291c6dbbf85af549247bff950e8343f7fd7",
          "url": "https://github.com/ocaml/dune/commit/d920ff4e9bb0cab87b50ba968383ecc6a50e6c95"
        },
        "date": 1679359787452,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic watch build time (warm, Linux)",
            "value": "31.061128806013333",
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
          "id": "962222d0667d94cf9cf6909cf80ec99cd14159e0",
          "message": "test: improve test configuration (#7358)\n\n- remove outsdated enabled_if since we don't test on old versions anymore\r\n- add missing dependencies on ppxlib\r\n\r\nSigned-off-by: Rudi Grinberg <me@rgrinberg.com>",
          "timestamp": "2023-03-20T17:59:55-07:00",
          "tree_id": "06d41911f228a4fb5f937bef749c6778d5f8044d",
          "url": "https://github.com/ocaml/dune/commit/962222d0667d94cf9cf6909cf80ec99cd14159e0"
        },
        "date": 1679361471314,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "synthetic watch build time (warm, Linux)",
            "value": "43.80490859539333",
            "unit": "seconds"
          }
        ]
      }
    ]
  }
}