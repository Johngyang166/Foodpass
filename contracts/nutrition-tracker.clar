;; Nutrition Tracker Contract
;; Tracks food purchases and nutritional intake for better distribution planning

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_FOOD_ITEM_NOT_FOUND (err u201))
(define-constant ERR_INVALID_QUANTITY (err u202))
(define-constant ERR_INVALID_NUTRITION_DATA (err u203))
(define-constant ERR_PURCHASE_NOT_FOUND (err u204))
(define-constant ERR_FOOD_CATEGORY_INVALID (err u205))
(define-constant ERR_BENEFICIARY_NOT_FOUND (err u206))
(define-constant ERR_NUTRITION_GOAL_INVALID (err u207))

;; Food categories
(define-constant CATEGORY_GRAINS u1)
(define-constant CATEGORY_VEGETABLES u2)
(define-constant CATEGORY_FRUITS u3)
(define-constant CATEGORY_PROTEINS u4)
(define-constant CATEGORY_DAIRY u5)
(define-constant CATEGORY_OILS u6)

;; Nutritional goals (per day in grams)
(define-constant DAILY_PROTEIN_GOAL u50)
(define-constant DAILY_CARBS_GOAL u300)
(define-constant DAILY_FAT_GOAL u65)
(define-constant DAILY_FIBER_GOAL u25)

;; Data variables
(define-data-var total-food-items uint u0)
(define-data-var total-purchases uint u0)
(define-data-var total-nutrition-entries uint u0)

;; Food item database
(define-map food-items
  uint
  {
    name: (string-ascii 50),
    category: uint,
    calories-per-100g: uint,
    protein-per-100g: uint,
    carbs-per-100g: uint,
    fat-per-100g: uint,
    fiber-per-100g: uint,
    is-active: bool,
    added-date: uint
  }
)

;; Food purchase logs
(define-map purchase-logs
  uint
  {
    beneficiary: principal,
    vendor-address: principal,
    food-item-id: uint,
    quantity-grams: uint,
    tokens-spent: uint,
    purchase-date: uint,
    nutritional-logged: bool
  }
)

;; Daily nutrition tracking per beneficiary
(define-map daily-nutrition
  { beneficiary: principal, date: uint }
  {
    total-calories: uint,
    total-protein: uint,
    total-carbs: uint,
    total-fat: uint,
    total-fiber: uint,
    meals-logged: uint,
    last-updated: uint
  }
)

;; Weekly nutrition summary
(define-map weekly-nutrition-summary
  { beneficiary: principal, week: uint }
  {
    avg-calories: uint,
    avg-protein: uint,
    avg-carbs: uint,
    avg-fat: uint,
    avg-fiber: uint,
    days-tracked: uint,
    week-start: uint
  }
)

;; Nutritional goals per beneficiary (customizable)
(define-map nutrition-goals
  principal
  {
    daily-calories: uint,
    daily-protein: uint,
    daily-carbs: uint,
    daily-fat: uint,
    daily-fiber: uint,
    family-size: uint,
    goal-set-date: uint
  }
)

;; Food category consumption analytics
(define-map category-consumption
  { beneficiary: principal, category: uint, month: uint }
  {
    total-quantity: uint,
    total-spent: uint,
    purchase-count: uint,
    avg-nutrition-score: uint
  }
)

;; Add food items to database (admin only)
(define-public (add-food-item 
  (name (string-ascii 50))
  (category uint)
  (calories uint)
  (protein uint)
  (carbs uint)
  (fat uint)
  (fiber uint))
  (let
    (
      (food-id (+ (var-get total-food-items) u1))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (and (>= category CATEGORY_GRAINS) (<= category CATEGORY_OILS)) ERR_FOOD_CATEGORY_INVALID)
    (asserts! (> calories u0) ERR_INVALID_NUTRITION_DATA)
    
    (map-set food-items food-id
      {
        name: name,
        category: category,
        calories-per-100g: calories,
        protein-per-100g: protein,
        carbs-per-100g: carbs,
        fat-per-100g: fat,
        fiber-per-100g: fiber,
        is-active: true,
        added-date: stacks-block-height
      }
    )
    
    (var-set total-food-items food-id)
    (ok food-id)
  )
)

;; Log food purchase by beneficiary
(define-public (log-food-purchase 
  (vendor-address principal)
  (food-item-id uint)
  (quantity-grams uint)
  (tokens-spent uint))
  (let
    (
      (purchase-id (+ (var-get total-purchases) u1))
      (food-item (unwrap! (map-get? food-items food-item-id) ERR_FOOD_ITEM_NOT_FOUND))
    )
    (asserts! (> quantity-grams u0) ERR_INVALID_QUANTITY)
    (asserts! (> tokens-spent u0) ERR_INVALID_QUANTITY)
    (asserts! (get is-active food-item) ERR_FOOD_ITEM_NOT_FOUND)
    
    (map-set purchase-logs purchase-id
      {
        beneficiary: tx-sender,
        vendor-address: vendor-address,
        food-item-id: food-item-id,
        quantity-grams: quantity-grams,
        tokens-spent: tokens-spent,
        purchase-date: stacks-block-height,
        nutritional-logged: false
      }
    )
    
    (var-set total-purchases purchase-id)
    (ok purchase-id)
  )
)

;; Calculate and log nutritional intake from purchase
(define-public (log-nutrition-from-purchase (purchase-id uint))
  (let
    (
      (purchase (unwrap! (map-get? purchase-logs purchase-id) ERR_PURCHASE_NOT_FOUND))
      (food-item (unwrap! (map-get? food-items (get food-item-id purchase)) ERR_FOOD_ITEM_NOT_FOUND))
      (beneficiary (get beneficiary purchase))
      (quantity (get quantity-grams purchase))
      (current-date (/ stacks-block-height u144)) ;; Blocks per day approximation
      (current-month (/ current-date u30))
    )
    (asserts! (is-eq tx-sender beneficiary) ERR_UNAUTHORIZED)
    (asserts! (not (get nutritional-logged purchase)) ERR_PURCHASE_NOT_FOUND)
    
    ;; Calculate nutritional values
    (let
      (
        (calories (/ (* (get calories-per-100g food-item) quantity) u100))
        (protein (/ (* (get protein-per-100g food-item) quantity) u100))
        (carbs (/ (* (get carbs-per-100g food-item) quantity) u100))
        (fat (/ (* (get fat-per-100g food-item) quantity) u100))
        (fiber (/ (* (get fiber-per-100g food-item) quantity) u100))
        (current-daily (default-to 
          { total-calories: u0, total-protein: u0, total-carbs: u0, total-fat: u0, total-fiber: u0, meals-logged: u0, last-updated: u0 }
          (map-get? daily-nutrition { beneficiary: beneficiary, date: current-date })))
      )
      
      ;; Update daily nutrition
      (map-set daily-nutrition { beneficiary: beneficiary, date: current-date }
        {
          total-calories: (+ (get total-calories current-daily) calories),
          total-protein: (+ (get total-protein current-daily) protein),
          total-carbs: (+ (get total-carbs current-daily) carbs),
          total-fat: (+ (get total-fat current-daily) fat),
          total-fiber: (+ (get total-fiber current-daily) fiber),
          meals-logged: (+ (get meals-logged current-daily) u1),
          last-updated: stacks-block-height
        }
      )
      
      ;; Update category consumption
      (let
        (
          (current-category (default-to 
            { total-quantity: u0, total-spent: u0, purchase-count: u0, avg-nutrition-score: u0 }
            (map-get? category-consumption { beneficiary: beneficiary, category: (get category food-item), month: current-month })))
        )
        (map-set category-consumption { beneficiary: beneficiary, category: (get category food-item), month: current-month }
          {
            total-quantity: (+ (get total-quantity current-category) quantity),
            total-spent: (+ (get total-spent current-category) (get tokens-spent purchase)),
            purchase-count: (+ (get purchase-count current-category) u1),
            avg-nutrition-score: (/ (+ calories protein carbs) u3) ;; Simple nutrition score
          }
        )
      )
      
      ;; Mark purchase as logged
      (map-set purchase-logs purchase-id
        (merge purchase { nutritional-logged: true })
      )
      
      (ok { calories: calories, protein: protein, carbs: carbs, fat: fat, fiber: fiber })
    )
  )
)

;; Set personalized nutrition goals
(define-public (set-nutrition-goals 
  (calories uint)
  (protein uint)
  (carbs uint)
  (fat uint)
  (fiber uint)
  (family-size uint))
  (begin
    (asserts! (> calories u0) ERR_NUTRITION_GOAL_INVALID)
    (asserts! (> family-size u0) ERR_NUTRITION_GOAL_INVALID)
    
    (map-set nutrition-goals tx-sender
      {
        daily-calories: calories,
        daily-protein: protein,
        daily-carbs: carbs,
        daily-fat: fat,
        daily-fiber: fiber,
        family-size: family-size,
        goal-set-date: stacks-block-height
      }
    )
    (ok true)
  )
)

;; Calculate weekly nutrition summary
(define-public (calculate-weekly-summary (beneficiary principal) (week uint))
  (let
    (
      (week-start (* week u7)) ;; Week start in days
      (summary-data (default-to 
        { avg-calories: u0, avg-protein: u0, avg-carbs: u0, avg-fat: u0, avg-fiber: u0, days-tracked: u0, week-start: week-start }
        (map-get? weekly-nutrition-summary { beneficiary: beneficiary, week: week })))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    
    ;; Simple calculation - in reality would aggregate daily data
    (map-set weekly-nutrition-summary { beneficiary: beneficiary, week: week }
      (merge summary-data { days-tracked: u7 }) ;; Simplified for demo
    )
    
    (ok true)
  )
)

;; Read-only functions

(define-read-only (get-food-item (food-id uint))
  (map-get? food-items food-id)
)

(define-read-only (get-purchase-log (purchase-id uint))
  (map-get? purchase-logs purchase-id)
)

(define-read-only (get-daily-nutrition (beneficiary principal) (date uint))
  (map-get? daily-nutrition { beneficiary: beneficiary, date: date })
)

(define-read-only (get-nutrition-goals (beneficiary principal))
  (map-get? nutrition-goals beneficiary)
)

(define-read-only (get-category-consumption (beneficiary principal) (category uint) (month uint))
  (map-get? category-consumption { beneficiary: beneficiary, category: category, month: month })
)

(define-read-only (get-weekly-summary (beneficiary principal) (week uint))
  (map-get? weekly-nutrition-summary { beneficiary: beneficiary, week: week })
)

(define-read-only (get-total-food-items)
  (var-get total-food-items)
)

(define-read-only (get-total-purchases)
  (var-get total-purchases)
)

(define-read-only (calculate-nutrition-score (beneficiary principal) (date uint))
  (let
    (
      (daily-data (map-get? daily-nutrition { beneficiary: beneficiary, date: date }))
      (goals (map-get? nutrition-goals beneficiary))
    )
    (match daily-data
      data 
        (match goals
          goal-data
            (let
              (
                (protein-score (if (>= (get total-protein data) (get daily-protein goal-data)) u100 
                                 (/ (* (get total-protein data) u100) (get daily-protein goal-data))))
                (carb-score (if (>= (get total-carbs data) (get daily-carbs goal-data)) u100
                              (/ (* (get total-carbs data) u100) (get daily-carbs goal-data))))
                (overall-score (/ (+ protein-score carb-score) u2))
              )
              (some overall-score)
            )
          none)
      none)
  )
)

(define-read-only (get-food-category-name (category uint))
  (if (is-eq category CATEGORY_GRAINS) "Grains"
    (if (is-eq category CATEGORY_VEGETABLES) "Vegetables"
      (if (is-eq category CATEGORY_FRUITS) "Fruits"
        (if (is-eq category CATEGORY_PROTEINS) "Proteins"
          (if (is-eq category CATEGORY_DAIRY) "Dairy"
            (if (is-eq category CATEGORY_OILS) "Oils"
              "Unknown"))))))
)
