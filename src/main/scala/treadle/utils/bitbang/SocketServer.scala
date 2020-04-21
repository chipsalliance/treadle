/*
Copyright 2020 The Regents of the University of California (Regents)

Licensed under the Apache License, Version 2.0 (the "License")
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

package treadle.utils.bitbang

import java.io._
import java.net.{InetAddress, ServerSocket, Socket}

object MyServer {
  def main(args: Array[String]) {
    if (args.length < 1) {
      printf("scala MyServer port")
      return
    }

    val port: Int = args(0).toInt

    try {
      println("This emulator compiled with JTAG Remote Bitbang client. To enable, use +jtag_rbb_enable=1.")
      println(s"Listening on port $port")
      val listener = new ServerSocket(port)

      val socket = listener.accept()

      val out = new PrintWriter(socket.getOutputStream)
      val in = new BufferedReader(new InputStreamReader(socket.getInputStream))

      val message = in.readLine()
      println("Here message: " + message)

      out.println("I got your message")

      out.close()
      in.close()
      socket.close()

      listener.close()
    } catch {
      case _: IOException =>
        System.err.println(s"Could not listen on port: $port.")
        System.exit(-1)
    }
  }
}

object MyClient {
  def main(args: Array[String]) {
    if (args.length < 2) {
      printf("scala MyClient hostname port")
      return
    }

    val host = args(0)
    val port: Int = args(1).toInt

    try {
      val ia = InetAddress.getByName(host)
      val socket = new Socket(ia, port)
      val out = new PrintWriter(socket.getOutputStream)
      val in = new BufferedReader(new InputStreamReader(socket.getInputStream))

      val message = scala.io.StdIn.readLine()
      out.println(message)
      out.flush()

      val receipt = in.readLine()
      println(receipt)

      out.close()
      in.close()
      socket.close()
    } catch {
      case e: IOException =>
        e.printStackTrace()
    }
  }
}