###############################
##區塊鏈的基本架構：
##
##

# 區塊(block)：儲存資料的容器
# 如果是交易資料，有新的交易就會產生新的資料、誕生出新的區塊，
# 而這個新的區塊便會與其他既有的區塊，鏈結在一起。

my_block <- list(
  index = 1,
  timestamp = Sys.time(),
  data = "some data",
  previous_hash = "0",
  proof = 1,
  new_hash = NULL
)
my_block

# Hash：sha256加密演算法
library("digest")
digest("R", "sha256")
digest(c("R", "Python"), "sha256")

# 將儲存資料的區塊加密，把區塊的內容物，和前一個區塊一併加密
get_hashed_block <- function(block){
	to_be_hashed <- c(
		block$index,
		block$timestamp,
		block$data,
		block$previous_hash
	)
	
	block$new_hash <- digest(to_be_hashed, "sha256")
	return(block)
}

# Proof-of-Work：以比特幣為例，就是比特幣的鑄造成本
# 如果建立比特幣的鑄造成本太低，會使幣值下滑太快，
# 因此此演算法要難以建立但容易驗證。
# ex: 從前一個區塊的proof+1開始找起，一直到同時滿足兩個條件：
# 1.可以被87整除  2.可以被前一個區塊的proof整除
proof_of_work <- function(last_proof){
	proof <- last_proof + 1

	while(!(proof%%87 == 0 & proof%%last_proof == 0)){
		proof <- proof + 1
	}
	
	return(proof)
}

# 以比特幣而言，比特幣的區塊鏈後端找到下一個proof數字、
# 建立新的區塊的行為，就是俗稱的挖礦，而挖礦的獎勵就是比特幣；

###############################
##正式建立區塊鏈
##
##

# 建立區塊的函數：
create_new_block <- function(previous_block){
	new_proof <- proof_of_work(previous_block$proof)
	new_block <- list(index = previous_block$index + 1,
				timestamp = Sys.time(),
				data = sprintf("這是第 %s 個區塊", previous_block$index + 1),
				previous_hash = previous_block$new_hash,
				proof = new_proof
				)
	new_block_hashed <- get_hashed_block(new_block)
	return(new_block_hashed)
}

# 第一個區塊：Genesis Block
# 因為建立區塊需要有一個previous block，因此要先建立一個區塊鏈的起點。
genesis_block <- list(
	index = 1,
	timestamp = Sys.time(),
	data = "Genesis Block", 
	previous_hash = "0",
	new_hash = NULL,
	proof = 1
)

# 建立區塊鏈：產出指定區塊數的迷你區塊鏈
build_blockchain <- function(num_of_blocks){
	# Genesis Block				##自行創造區塊鏈中的第一個區塊
	genesis_block <- list(
		index = 1,				##包含index: 這是區塊的編號
		timestamp = Sys.time(),		##時間戳記: 何時建立的
		data = "Genesis Block",		##你要記錄在區塊中的資料，如果是交易資料，可以令： 
							##data = c("交易人", "交易金額", "交易對象")
		previous_hash = "0",		##前一個區塊的hash值，因為第一個區塊沒有前一個區塊，所以為0
		new_hash = NULL,			##新的hash值，會在產生新區塊之後才被建立
		proof = 1				##初始化proof的值為1，用以進行proof-of-work演算法來產生區塊
	)

	# Build time				##這是用來作圖的，不必要
	build_times <- vector()			##創建一個向量，來放置建立所需的時間
  
	# Build blockchain
	mini_blockchain <- list(genesis_block)		##先令我們的迷你區塊鏈裡面，只有第一個區塊: genesis block
	previous_block <- mini_blockchain[[1]]		##因為要建立第二個區塊，
									##因此令previous block為我們迷你區塊鏈的第一項
									##在這裡用雙中括號，因為genesis block是mini_blockchain這個list中的第1個list
	num_of_blocks_to_add <- num_of_blocks - 1		##假設我們要建立20個區塊，那麼扣除第一個genesis block，我們還要建立19個額外的區塊
  
	for (i in 1:num_of_blocks_to_add){			##執行for迴圈，每一次迴圈代表建立一個區塊
		system_time <- system.time(			##取得創建新區塊當下的時間(作圖用)
		block_to_add <- create_new_block(previous_block))	##利用前一個區塊，創建新區塊
		system_time <- unname(system_time)[3]			##只是移除dimension名稱
		mini_blockchain[[i + 1]] <- block_to_add			##把創建的新區塊，放到我們的迷你區塊鏈裡面
		previous_block <- block_to_add				##成功放入後，現在創建的新區塊要變成下一個新區塊的previous block
		build_times <- c(build_times, system_time)		##我們要的是，建立一個區塊的所需時間，而不是當下的時間
											##因此我們在這裡把當下的時間，轉換成所需時間
		writeLines(										##這裡可以不寫，寫了只是讓你方便觀察
			sprintf("區塊 %s 已經被加入\n\tProof: %s \n\t Hash: %s",		##讓你知道現在區塊鏈已經加入第幾個區塊、並加入哪些東西
			block_to_add$index, block_to_add$proof, block_to_add$new_hash))
	}
  
	return(list(mini_blockchain, build_times))			##等所有區塊都被加入後(for迴圈全部跑完後)，
											##把我們的整個迷你區塊鏈和所需時間回傳(再聲明一次，所需時間不必要，是作圖用)
}

# Call function			##建立好整個迷你區塊鏈後，來試試看吧
num_blocks <- 20			##設定我們要加入的區塊數為20
my_mini_blockchain <- build_blockchain(num_blocks)		##開始建立迷你區塊鏈
mini_blockchain <- my_mini_blockchain[[1]]			##因為最後回傳list(迷你區塊鏈, 所需時間)，因此這裡迷你區塊鏈[[1]]就是我們的區塊鏈主體
build_times <- my_mini_blockchain[[2]]				##而迷你區塊鏈[[2]]就是所需時間

###############################
##利用ggplot2作圖：觀察每個區塊建立的所需時間
##
##
library(ggplot2)									##ggplot2是R的作圖套件，是圖層概念(一共有7層)
bc_df <- data.frame(block_index = 2:num_blocks, build_times)	##要建立的圖是：每個區塊的所需時間
											##因此X軸為: 區塊數；Y軸為所需時間
ggplot(bc_df, aes(x = block_index, y = build_times)) +		##第一個引數是資料集，aes是美學aesthetics的縮寫，也就是外觀設定
	geom_line(size = 1.5) +							##想要在圖裡面加上什麼圖層，就直接用"+"號，geom就是幾何圖形
											##aes和geom的差別在於，aes管理座標軸、顏色變化等，geom是點線面作圖
	xlab("Block Index") +							##xlab是X軸名稱
	ylab("Time Elapsed(sec)") +						##ylab是Y軸名稱
	ggtitle("Time to Build a New Block") +				##ggtitle就是標題
	theme_minimal()								##表示整個圖是極簡風格，沒有任何background





