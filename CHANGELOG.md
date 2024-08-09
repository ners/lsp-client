# Revision history for lsp-client

## 0.4.0.0  -- 2024-08-09

* Update to lsp-2.7 and lsp-types-2.3
* Introduce SessionT and MonadSession
* Lift all functions to MonadSession
* Session.initialize now takes LSP initialization options and returns the initialization result
* Session state no longer holds initialization result
* Add Session.getAllVersionedDocs

## 0.3.0.0  -- 2024-04-04

* Support lsp 2.4

## 0.2.0.0  -- 2023-11-01

* Support lsp-types 2.0
* Move compat functionality to unix-compat >= 0.7.1

## 0.1.0.0  -- 2023-06-20

* First version.
