.. image:: https://github.com/countvajhula/virtual-ring/actions/workflows/test.yml/badge.svg
    :target: https://github.com/countvajhula/virtual-ring/actions

.. image:: https://coveralls.io/repos/github/countvajhula/virtual-ring/badge.svg?branch=main
    :target: https://coveralls.io/github/countvajhula/virtual-ring?branch=main

.. image:: https://melpa.org/packages/virtual-ring-badge.svg
    :alt: MELPA
    :target: https://melpa.org/#/virtual-ring

.. image:: https://stable.melpa.org/packages/virtual-ring-badge.svg
    :alt: MELPA Stable
    :target: https://stable.melpa.org/#/virtual-ring

virtual-ring
============

A thin wrapper around Emacs's ring data structure that layers stateful, "virtual," rotation on top of the underlying fixed-size recency-aware ring.

Virtual rings are a good fit in cases where you need to keep track both of recency of insertion as well as have an independent notion of stateful rotation to track a current "selection."

Installation
------------

Virtual-ring is on `MELPA <https://melpa.org/>`_. You can install it in the usual way using your package manager of choice (e.g., `Straight.el <https://github.com/radian-software/straight.el>`_, `Elpaca <https://github.com/progfolio/elpaca>`_, or Emacs's built-in package.el), after ensuring you have MELPA in your configured list of package archives.

Usage
-----

.. code-block:: elisp

  ;; Create a new virtual ring that can hold 5 items
  (setq my-history (virtual-ring-make 5))

  ;; Store some items
  (virtual-ring-store my-history "alpha")
  (virtual-ring-store my-history "beta")
  (virtual-ring-store my-history "gamma")

  ;; "gamma" is both the most recently added AND the current entry
  (virtual-ring-last-entry my-history) ; => "gamma"
  (virtual-ring-current-entry my-history) ; => "gamma"

  ;; Now, let's navigate the ring like a user pressing M-p (in a LIFO ring)
  (virtual-ring-rotate-backwards my-history)

  ;; The current selection has changed, but the most recent item is still the same.
  (virtual-ring-current-entry my-history) ; => "beta"
  (virtual-ring-last-entry my-history) ; => "gamma"

  ;; Storing a new item resets the virtual head automatically, by default
  (virtual-ring-store my-history "delta")

  (virtual-ring-current-entry my-history) ; => "delta"
  (virtual-ring-head my-history) ; => 0

Non-Ownership
-------------

The freely released, copyright-free work in this repository represents an investment in a better way of doing things called attribution-based economics. Attribution-based economics is based on the simple idea that we gain more by giving more, not by holding on to things that, truly, we could only create because we, in our turn, received from others. As it turns out, an economic system based on attribution -- where those who give more are more empowered -- is significantly more efficient than capitalism while also being stable and fair (unlike capitalism, on both counts), giving it transformative power to elevate the human condition and address the problems that face us today along with a host of others that have been intractable since the beginning. You can help make this a reality by releasing your work in the same way -- freely into the public domain in the simple hope of providing value. Learn more about attribution-based economics at `drym.org <https://drym.org>`_, tell your friends, do your part.
