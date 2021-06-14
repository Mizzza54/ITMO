package info.kgeorgiy.ja.gerasimov.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * @author Michael Gerasimov
 * start: 15.05.2021
 * @version -
 */
public class HelloUDPServer implements HelloServer {
    private DatagramSocket socket;
    private ExecutorService workers;
    private ExecutorService listener;

    @Override
    public void start(int port, int threads) {
        try {
            socket = new DatagramSocket(port);
            workers = Executors.newFixedThreadPool(threads);
            listener = Executors.newSingleThreadExecutor();
            int size = socket.getSendBufferSize();
            listener.submit(() -> createListenTask(size));
        } catch (SocketException e) {
            System.err.println("Socket could not be opened");
        }
    }

    private void createResponseTask(final DatagramPacket packet) {
        try {
            packet.setData(String.format("Hello, %s", new String(packet.getData(), packet.getOffset(),
                    packet.getLength(), StandardCharsets.UTF_8)).getBytes(StandardCharsets.UTF_8));
            socket.send(packet);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void createListenTask(final int size) {
        while (!socket.isClosed() && !Thread.interrupted()) {
            DatagramPacket packet = new DatagramPacket(new byte[size], size);
            try {
                socket.receive(packet);
                workers.submit(() ->createResponseTask(packet));
            } catch (IOException e) {
                System.err.println("Socket closed");
            }
        }
    }

    @Override
    public void close() {
        socket.close();
        Common.shutdownAndAwaitTermination(workers);
        Common.shutdownAndAwaitTermination(listener);
    }

    public static void main(String[] args) {
        Common.mainServer(args, new HelloUDPServer());
    }
}
