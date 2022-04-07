package ctci.bits

object ch5:

    def getBit(n: Int, i: Int): Boolean =
        (n & (1 << i)) != 0

    def setBit(n: Int, i: Int): Int =
        n | (1 << i)

    def clearBit(n: Int, i: Int): Int =
        val mask = ~(1 << i)
        n & mask

    def updateBit(n: Int, i: Int, isBitSet: Boolean): Int = 
        if isBitSet then
            setBit(n, i)
        else clearBit(n, i)

    def convertToBitSeq(n: Int): Seq[Boolean] =
        (0 until 32).map(i => getBit(n,i))

    def getBitString(n: Int, dropLeadZeros: Boolean = true): String =

        val bits = convertToBitSeq(n).map(i => if i then "1" else "0").reverse
        
        val bitsOut = 
            if dropLeadZeros then 
                bits.dropWhile(_ == "0")
            else
                bits
        
        bitsOut.mkString

    def insert(n: Int, m: Int, i: Int, j: Int): Int =
        /* 
        Insert m into n, such that m starts at j and ends at i
         */

        val mask = ((-1) << (j+1)) | ((1 << i) - 1)
        (n & mask) | (m << i)

    def flipToWin(n: Int): (Int, Int) = 

        val bits = convertToBitSeq(n).toArray

        var i = 0
        var maxRun = -1
        var flip = 0

        while i < bits.length do

            val bit = getBit(n, i)

            var numOnes = 1
            var j = i + 1 
            var inRun = true
            var bitToFlip = if !bit then Option(i) else None

            while j < bits.length && inRun do 
                if bits(j) then 
                    numOnes += 1
                    j += 1
                else
                    if bitToFlip.isEmpty then
                        bitToFlip = Option(j)
                        numOnes += 1
                        j += 1
                    else 
                        inRun = false
        
            if bitToFlip.isDefined then 
                if numOnes > maxRun then 
                    maxRun = numOnes 
                    flip = bitToFlip.get
                i = bitToFlip.get + 1
            else
                i = bits.length

        (flip, maxRun)


    def conversion(n: Int, m: Int): Int =

        // Use XOR to determine which bits are different in m and n
        var bitFlips = n ^ m 

        // Count the 1s in the bitFlipMask
        var numBitsToFlip = 0
        while bitFlips != 0 do
            //println(s"bitflips: ${getBitString(bitFlips)}")
            numBitsToFlip += bitFlips & 1
            bitFlips >>>= 1
            

        numBitsToFlip

    @main def TryCh5 =

        val n = Integer.parseInt("10000000001",2)
        val m = Integer.parseInt("1011",2)
        val expectedOutcome = "10000101101"

        println(s"Expected: $expectedOutcome")
        println(s"Observed: ${getBitString(insert(n,m,2,6))}")


        println(getBitString(1775))
        println(flipToWin(1775))


        val x = 29
        val y = 15
        println(s"Conversion from ${getBitString(x, false)} to ${getBitString(y, false)}: ${conversion(x,y)}")