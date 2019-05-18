import com.roundeights.hasher.Implicits._
import scala.language.postfixOps
import scala.collection.mutable.ListBuffer
import scala.util.Random
import java.util.Date
import com.google.gson._
import spark.Spark._
import scalaj.http._
import collection.JavaConverters._

class Transaction(
	val sender: String, 
	val recipient: String,
	val amount: Long
)

case class Block(
	val index: Int,
	val timestamp: Long,
	val transactions: List[Transaction],
	val proof: Long,
	val prevHash: String
)

class ChainResp(
	val chain: java.util.List[Block],
	val length: Int
)

class Chain {

	var chain = new ListBuffer[Block]()
	var currentTransactions = new ListBuffer[Transaction]()
	var nodes: Set[String] = Set()
	newBlock(1, "test")
	newBlock(2, "test")
	
	def newBlock = (proof: Long, prevHash: String) => {
		val newBlock = new Block(
			index = chain.length + 1,
			timestamp = new Date().getTime(),
			transactions = this.currentTransactions.toList,
			proof = proof,
			prevHash = prevHash
		)

		currentTransactions.clear
		chain += newBlock
		
		newBlock
	}

	def newTransaction = (tr: Transaction) => {
		currentTransactions += tr
		
		lastBlock().index
	}

	def hash = (block: Block) => {
		new Gson()
			.toJson(block)
			.sha256
			.hex
	}

	def lastBlock = () => {
		chain(chain.length - 1)
	}

	def pow = (prevProof: Long) => {
		def proof (p: Long): Long = 
			if(validate(p)) p else proof(p + 1)

		proof(prevProof)
	}

	def validate = (p: Long) => {
		s"$p".sha256.hex.startsWith("69")
	}

	def registerNode = (address: String) => {
		nodes += address
	}

	def validateChain = (chain: List[Block]) => {
		var lastBlock = chain(0)
	
		def loop (i: Int): Boolean = {
			if(i == chain.length) true 
			else {
				val block = chain(i)
				if(block.prevHash != hash(lastBlock))
					false
				if(!validate(lastBlock.proof))
					false

				lastBlock = block
				loop(i + 1)
			}
		}

		loop(1)
	}

	def consensus = () => {
		var newChain: List[Block] = List()
		var maxLen = chain.length

		nodes.foreach(el => {
			val res = Http(s"$el/chain").asString
			if(res.code == 200){
				val b = new Gson()
						.fromJson(
							res.body,
							classOf[ChainResp]
						)
				val length = b.length
				val chain = b.chain.asScala.toList

				if(length > maxLen && validateChain(chain)){
					maxLen = length
					newChain = chain
				}
			}
		})
		if(newChain != null){
			chain = newChain.to[ListBuffer]
			true
		}
		false
	}
}

object Main extends App {
	val uuid = (Random.nextInt
				(999999999 - 100000000) 
				+ 1000000000
				+ ""
				).sha256
				.hex
	val blockchain = new Chain()

	port(9001)
	get("/mine", (req, res) => {
		val lastBlock = blockchain.lastBlock()
		val lastProof = lastBlock.proof
		val proof = blockchain.pow(lastProof)

		val transaction = new Transaction(
			sender = "0",
			recipient = uuid,
			amount = 1
		)
		blockchain.newTransaction(transaction)

		val prevHash = blockchain.hash(lastBlock)
		val block = blockchain.newBlock(proof, prevHash)
		val transJson = new Gson().toJson(block.transactions)

		s"""{
		"msg" : "New block forged",
		"index" : ${block.index},
		"transactions" : ${transJson},
		"proof" : ${block.proof},
		"prev_hash" : ${block.prevHash}
		}"""
	})
	post("/newtransaction", (req, res) => {
		val bC = new JsonParser().parse(req.body).getAsJsonObject()

		val transaction = new Transaction(
			sender = bC.get("sender").getAsString(),
			recipient = bC.get("recipient").getAsString(),
			amount = bC.get("amount").getAsLong()
		)
		val index = blockchain.newTransaction(transaction)

		s"transaction added to index ${index}"
	})
	get("/chain", (req, res) => {
		val ch = blockchain
					.chain
					.toList
					.asJava
					
		val resp = new ChainResp(
			chain = ch,
			length = blockchain.chain.length
		)
		new Gson().toJson(resp)
	})
	post("nodes/register", (req, res) => {
		val node = new JsonParser()
					.parse(req.body)
					.getAsJsonObject()
					.get("address")
					.getAsString
		blockchain.registerNode(node)
		"ok"
	})
	get("/nodes/resolve", (req, res) => {
		val chain = new Gson().toJson(blockchain.chain.toList.asJava)
		if(blockchain.consensus()){
			s"""
			{
			"message" : "Our chain was replaced",
			"chain" : ${chain}
			}
			"""
		}else{
			s"""
			{
			"message" : "Our chain is authoritative",
			"chain" : ${chain}
			}
			"""
		}
	})
}
