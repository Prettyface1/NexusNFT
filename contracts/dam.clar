;; NexusNFT Marketplace Contract
;; This contract implements core marketplace functionality for NFT trading

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-listing-not-found (err u102))
(define-constant err-wrong-price (err u103))
(define-constant err-invalid-token (err u104))
(define-constant err-invalid-price (err u105))
(define-constant err-invalid-metadata (err u106))
(define-constant err-self-transfer (err u107))

;; Data Maps
(define-map listings
  { token-id: uint }
  { price: uint, seller: principal }
)

(define-map tokens
  { token-id: uint }
  { owner: principal, metadata-url: (string-utf8 256) }
)

(define-data-var next-token-id uint u0)

;; Private Functions
(define-private (is-valid-metadata-url (url (string-utf8 256)))
  (> (len url) u0)
)

(define-private (is-valid-price (price uint))
  (> price u0)
)

(define-private (is-valid-token (token-id uint))
  (is-some (map-get? tokens { token-id: token-id }))
)

;; Read-only functions
(define-read-only (get-token-owner (token-id uint))
  (match (map-get? tokens { token-id: token-id })
    entry (ok (get owner entry))
    (err u404)
  )
)

(define-read-only (get-listing (token-id uint))
  (map-get? listings { token-id: token-id })
)

(define-read-only (get-token-metadata (token-id uint))
  (match (map-get? tokens { token-id: token-id })
    entry (ok (get metadata-url entry))
    (err u404)
  )
)

;; Public functions
(define-public (mint (metadata-url (string-utf8 256)))
  (begin
    (asserts! (is-valid-metadata-url metadata-url) err-invalid-metadata)
    (let ((token-id (var-get next-token-id)))
      (map-set tokens
        { token-id: token-id }
        { owner: tx-sender, metadata-url: metadata-url }
      )
      (var-set next-token-id (+ token-id u1))
      (ok token-id)
    )
  )
)

(define-public (list-token (token-id uint) (price uint))
  (begin
    (asserts! (is-valid-token token-id) err-invalid-token)
    (asserts! (is-valid-price price) err-invalid-price)
    (let ((token-owner (unwrap! (get-token-owner token-id) (err u404))))
      (asserts! (is-eq token-owner tx-sender) err-not-token-owner)
      (ok (map-set listings { token-id: token-id } { price: price, seller: tx-sender }))
    )
  )
)

(define-public (unlist-token (token-id uint))
  (begin
    (asserts! (is-valid-token token-id) err-invalid-token)
    (let ((token-owner (unwrap! (get-token-owner token-id) (err u404))))
      (asserts! (is-eq token-owner tx-sender) err-not-token-owner)
      (ok (map-delete listings { token-id: token-id }))
    )
  )
)

(define-public (buy-token (token-id uint))
  (begin
    (asserts! (is-valid-token token-id) err-invalid-token)
    (let (
      (listing (unwrap! (get-listing token-id) err-listing-not-found))
      (price (get price listing))
      (seller (get seller listing))
      (token-data (unwrap! (map-get? tokens { token-id: token-id }) (err u404)))
    )
      (asserts! (not (is-eq tx-sender seller)) err-self-transfer)
      (try! (stx-transfer? price tx-sender seller))
      (map-delete listings { token-id: token-id })
      (map-set tokens 
        { token-id: token-id } 
        { owner: tx-sender, metadata-url: (get metadata-url token-data) }
      )
      (ok true)
    )
  )
)