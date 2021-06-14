package info.kgeorgiy.ja.gerasimov.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.util.Iterator;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static info.kgeorgiy.ja.gerasimov.hello.Common.*;

/**
 * @author Michael Gerasimov
 * start: 23.05.2021
 * @version -
 */
public class HelloUDPNonblockingServer implements HelloServer {
    private Selector selector;
    private ExecutorService executorService;
    private DatagramChannel channel;

    @Override
    public final void start(final int port, final int threads) {
        try {
            selector = Selector.open();
            channel = DatagramChannel.open();
            channel.configureBlocking(false);
            channel.bind(new InetSocketAddress(port));
            channel.register(selector,
                    SelectionKey.OP_READ,
                    new AttachmentServer(null, channel.socket().getReceiveBufferSize()));
        } catch (IllegalArgumentException e) {
            System.err.println("Bad host");
            return;
        } catch (IOException e) {
            System.err.println("Cant open channel or selector");
            return;
        }
        executorService = Executors.newSingleThreadExecutor();
        executorService.submit(() -> {
            try {
                while (!Thread.interrupted() && selector.isOpen()) {
                    selector.select();
                    for (final Iterator<SelectionKey> i = selector.selectedKeys().iterator(); i.hasNext(); ) {
                        try {
                            final SelectionKey key = i.next();
                            final DatagramChannel datagramChannel = (DatagramChannel) key.channel();
                            final AttachmentServer attachmentServer = (AttachmentServer) key.attachment();

                            if (key.isReadable()) {
                                handleRead(datagramChannel, attachmentServer);
                                key.interestOps(SelectionKey.OP_WRITE);
                            }

                            if (key.isWritable()) {
                                handleWrite(datagramChannel, attachmentServer);
                                key.interestOps(SelectionKey.OP_READ);
                            }
                        } finally {
                            i.remove();
                        }
                    }
                }
            } catch (final IOException e) {
                close();
                e.printStackTrace();
            }
        });
    }

    private void handleRead(final DatagramChannel datagramChannel,
                            final AttachmentServer attachmentServer) throws IOException {

        attachmentServer.setSocketAddress(receive(datagramChannel, attachmentServer.getByteBuffer()));
    }

    private void handleWrite(final DatagramChannel datagramChannel,
                             final AttachmentServer attachmentServer) throws IOException {

        if (attachmentServer.getSocketAddress() == null) {
            return;
        }

        putAndSend(datagramChannel,
                attachmentServer.getByteBuffer(),
                attachmentServer.getResponseData(),
                attachmentServer.getSocketAddress());
    }

    private static class AttachmentServer {
        private SocketAddress socketAddress;
        private final ByteBuffer byteBuffer;

        AttachmentServer(final SocketAddress socketAddress, final int bufferSize) {
            this.socketAddress = socketAddress;
            this.byteBuffer = ByteBuffer.allocate(bufferSize);
        }

        protected SocketAddress getSocketAddress() {
            return socketAddress;
        }

        protected void setSocketAddress(final SocketAddress socketAddress) {
            this.socketAddress = socketAddress;
        }

        protected ByteBuffer getByteBuffer() {
            return byteBuffer;
        }

        protected byte[] getResponseData() {
            return stringToBytes(String.format("Hello, %s", bytesToString(getByteBuffer())));
        }
    }

    @Override
    public final void close() {
        try {
            if (channel != null && channel.isOpen()) {
                channel.close();
            }
            if (selector != null && selector.isOpen()) {
                selector.close();
            }
        } catch (final IOException e) {
            System.err.println("Can not close channel/selector");
            e.printStackTrace();
        } finally {
            shutdownAndAwaitTermination(executorService);
        }
    }

    public static void main(final String[] args) {
        mainServer(args, new HelloUDPNonblockingServer());
    }
}
