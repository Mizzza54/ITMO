package info.kgeorgiy.ja.gerasimov.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.io.IOException;
import java.net.*;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.IntStream;

/**
 * @author Michael Gerasimov
 * start: 15.05.2021
 * @version -
 */
public class HelloUDPClient implements HelloClient {
    private final static int DEFAULT_SOCKET_TIMEOUT = 100;

    @Override
    public void run(String host, int port, String prefix, int threads, int requests) {
        SocketAddress address;
        try {
            address = new InetSocketAddress(InetAddress.getByName(host), port);
        } catch (UnknownHostException ignored) {
            System.err.println("Invalid host");
            return;
        }

        ExecutorService workers = Executors.newFixedThreadPool(threads);
        IntStream.range(0, threads).forEach(index -> workers.submit(() -> createTask(address, prefix, index, requests)));
        Common.shutdownAndAwaitTermination(workers);
    }

    private void createTask(final SocketAddress address,
                            final String prefix,
                            final int threadIndex,
                            final int request) {
        try (DatagramSocket socket = new DatagramSocket()) {
            socket.setSoTimeout(DEFAULT_SOCKET_TIMEOUT);
            DatagramPacket requestPacket = new DatagramPacket(new byte[0], 0, address);
            DatagramPacket receivePacket = new DatagramPacket(new byte[socket.getReceiveBufferSize()],
                    socket.getReceiveBufferSize());
            for (int requestIndex = 0; requestIndex < request; requestIndex++) {
                String message = String.format("%s%d_%d", prefix, threadIndex, requestIndex);
                while (!socket.isClosed() && !Thread.interrupted()) {
                    try {
                        requestPacket.setData(message.getBytes(StandardCharsets.UTF_8));
                        socket.send(requestPacket);
                        System.out.printf("%s\n", message);
                        socket.receive(receivePacket);
                        String receiveMessage = new String(receivePacket.getData(), receivePacket.getOffset(),
                                receivePacket.getLength(), StandardCharsets.UTF_8).trim();
                        if (receiveMessage.equals(String.format("Hello, %s", message))) {
                            System.out.printf("%s\n", receiveMessage);
                            break;
                        }

                    } catch (SocketTimeoutException e) {
                        System.err.println("Error: Timeout" );
                    } catch (IOException e) {
                        e.printStackTrace();
                    }

                }
            }
        } catch (SocketException e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        Common.mainClient(args, new HelloUDPClient());
    }
}
