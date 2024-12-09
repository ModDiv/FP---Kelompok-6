import Data.List (find)
import Data.Maybe (isNothing, fromJust)
import System.IO (hFlush, stdout)

-- Data Type Definitions
data Item = Item
  { itemId :: Int
  , itemName :: String
  , itemStock :: Int
  , itemPrice :: Double
  , itemCategory :: String
  , itemTax :: Double
  , itemDiscount :: Double
  } deriving (Show)

type Inventory = [Item]
type Cart = [(Item, Int)] -- (Item, Quantity)
type Transaction = [(Item, Int, Double)] -- (Item, Quantity, Total Price)

-- Helper Functions
findItemById :: Int -> Inventory -> Maybe Item
findItemById id inv = find (\item -> itemId item == id) inv

applyDiscount :: Item -> Double
applyDiscount item =
  let priceAfterDiscount = itemPrice item * (1 - itemDiscount item / 100)
  in priceAfterDiscount * (1 + itemTax item / 100)

-- Menambah barang ke inventory
addItem :: Inventory -> IO Inventory
addItem inv = do
  putStrLn "=== Tambah Barang ==="
  putStr "ID Barang: "
  hFlush stdout
  id <- readLn
  if isNothing (findItemById id inv)
    then do
      putStr "Nama Barang: "
      hFlush stdout
      name <- getLine
      putStr "Jumlah Barang: "
      hFlush stdout
      stock <- readLn
      putStr "Harga Barang: "
      hFlush stdout
      price <- readLn
      putStr "Kategori Barang: "
      hFlush stdout
      category <- getLine
      putStr "Pajak Barang (dalam %): "
      hFlush stdout
      tax <- readLn
      let newItem = Item id name stock price category tax 0
      return (inv ++ [newItem])
    else do
      putStrLn "Barang dengan ID tersebut sudah ada!"
      return inv

-- Melihat data barang
viewItems :: Inventory -> IO ()
viewItems inv = do
  putStrLn "\n=== Data Barang ==="
  mapM_ print inv
  putStrLn ""

-- Terapkan diskon
applyItemDiscount :: Inventory -> IO Inventory
applyItemDiscount inv = do
  putStr "ID Barang: "
  hFlush stdout
  id <- readLn
  let itemMaybe = findItemById id inv
  if isNothing itemMaybe
    then do
      putStrLn "Barang tidak ditemukan!"
      return inv
    else do
      putStr "Diskon (dalam %): "
      hFlush stdout
      discount <- readLn
      let updatedInv = map (\item -> if itemId item == id then item { itemDiscount = discount } else item) inv
      putStrLn "Diskon berhasil diterapkan!"
      return updatedInv

-- Menghapus diskon
removeItemDiscount :: Inventory -> IO Inventory
removeItemDiscount inv = do
  putStr "ID Barang: "
  hFlush stdout
  id <- readLn
  let itemMaybe = findItemById id inv
  if isNothing itemMaybe
    then do
      putStrLn "Barang tidak ditemukan!"
      return inv
    else do
      let updatedInv = map (\item -> if itemId item == id then item { itemDiscount = 0 } else item) inv
      putStrLn "Diskon berhasil dihapus!"
      return updatedInv

-- Edit atau hapus barang
editOrRemoveItem :: Inventory -> IO Inventory
editOrRemoveItem inv = do
  putStr "ID Barang: "
  hFlush stdout
  id <- readLn
  let itemMaybe = findItemById id inv
  if isNothing itemMaybe
    then do
      putStrLn "Barang tidak ditemukan!"
      return inv
    else do
      putStr "Pilih (1 = Edit, 2 = Hapus): "
      hFlush stdout
      choice <- readLn
      if choice == 1
        then do
          putStr "Nama Barang Baru: "
          hFlush stdout
          name <- getLine
          putStr "Kategori Baru: "
          hFlush stdout
          category <- getLine
          putStr "Harga Baru: "
          hFlush stdout
          price <- readLn
          putStr "Pajak Baru (dalam %): "
          hFlush stdout
          tax <- readLn
          let updatedInv = map (\item -> if itemId item == id then item { itemName = name, itemCategory = category, itemPrice = price, itemTax = tax } else item) inv
          putStrLn "Barang berhasil diubah!"
          return updatedInv
        else do
          let updatedInv = filter (\item -> itemId item /= id) inv
          putStrLn "Barang berhasil dihapus!"
          return updatedInv

-- Menambah barang ke keranjang
addToCart :: Inventory -> Cart -> IO (Inventory, Cart)
addToCart inv cart = do
  putStr "ID Barang: "
  hFlush stdout
  id <- readLn
  let itemMaybe = findItemById id inv
  if isNothing itemMaybe
    then do
      putStrLn "Barang tidak ditemukan!"
      return (inv, cart)
    else do
      putStr "Jumlah Barang: "
      hFlush stdout
      qty <- readLn
      let item = fromJust itemMaybe
      if itemStock item >= qty
        then do
          let updatedItem = item { itemStock = itemStock item - qty }
          let updatedInv = map (\i -> if itemId i == id then updatedItem else i) inv
          let updatedCart = cart ++ [(updatedItem, qty)]
          putStrLn "Barang berhasil ditambahkan ke keranjang!"
          return (updatedInv, updatedCart)
        else do
          putStrLn "Stok barang tidak mencukupi!"
          return (inv, cart)

-- Checkout
checkoutCart :: Cart -> IO Double
checkoutCart cart = do
  let total = sum [applyDiscount item * fromIntegral qty | (item, qty) <- cart]
  putStrLn $ "Total harga: " ++ show total
  return total

-- Program Utama
main :: IO ()
main = mainMenu [] [] []

mainMenu :: Inventory -> Cart -> [Transaction] -> IO ()
mainMenu inv cart history = do
  putStrLn "\n=== Program Kasir ==="
  putStrLn "1. Tambah Barang"
  putStrLn "2. Lihat Data Barang"
  putStrLn "3. Terapkan Diskon"
  putStrLn "4. Hapus Diskon"
  putStrLn "5. Edit/Hapus Barang"
  putStrLn "6. Tambah Barang ke Keranjang"
  putStrLn "7. Checkout"
  putStrLn "8. Lihat Riwayat Transaksi"
  putStrLn "9. Keluar"
  putStr "Pilih menu: "
  hFlush stdout
  choice <- readLn
  case choice of
    1 -> do newInv <- addItem inv
            mainMenu newInv cart history
    2 -> do viewItems inv
            mainMenu inv cart history
    3 -> do newInv <- applyItemDiscount inv
            mainMenu newInv cart history
    4 -> do newInv <- removeItemDiscount inv
            mainMenu newInv cart history
    5 -> do newInv <- editOrRemoveItem inv
            mainMenu newInv cart history
    6 -> do (newInv, newCart) <- addToCart inv cart
            mainMenu newInv newCart history
    7 -> do total <- checkoutCart cart
            let newHistory = history ++ [map (\(item, qty) -> (item, qty, applyDiscount item * fromIntegral qty)) cart]
            mainMenu inv [] newHistory
    8 -> do putStrLn "\n=== Riwayat Transaksi ==="
            mapM_ print history
            mainMenu inv cart history
    9 -> putStrLn "Terima kasih telah menggunakan program kasir!"
    _ -> do putStrLn "Pilihan tidak valid, coba lagi."
            mainMenu inv cart history
