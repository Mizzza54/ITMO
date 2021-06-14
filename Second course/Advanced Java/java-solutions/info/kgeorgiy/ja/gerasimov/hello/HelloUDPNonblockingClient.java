package info.kgeorgiy.ja.gerasimov.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.util.Iterator;

import static info.kgeorgiy.ja.gerasimov.hello.Common.*;

/**
 * @author Michael Gerasimov
 * start: 23.05.2021
 * @version -
 */
public class HelloUDPNonblockingClient implements HelloClient {
    private final static int TIMEOUT_MILLISECONDS = 100;

    @Override
    public final void run(final String host,
                          final int port,
                          final String prefix,
                          final int threads,
                          final int requests) {
        SocketAddress address;
        try {
            address = new InetSocketAddress(InetAddress.getByName(host), port);
        } catch (final UnknownHostException e) {
            e.printStackTrace();
            return;
        }

        try (final Selector selector = Selector.open()) {
            try {
                for (int threadIndex = 0; threadIndex < threads; threadIndex++) {
                    try {
                        final DatagramChannel datagramChannel = DatagramChannel.open();
                        datagramChannel.configureBlocking(false);
                        // :NOTE: NPE
                        datagramChannel.connect(address);
                        datagramChannel.register(
                                selector,
                                SelectionKey.OP_WRITE,
                                new AttachmentClient(
                                        threadIndex,
                                        0,
                                        datagramChannel.socket().getReceiveBufferSize(),
                                        prefix));

                    } catch (final IOException e) {
                        e.printStackTrace();
                        closeChannels(selector);
                        return;
                    }
                }

                while (!Thread.interrupted() && !selector.keys().isEmpty()) {

                    if (selector.select(TIMEOUT_MILLISECONDS) == 0) {
                        selector.keys().forEach(key -> key.interestOps(SelectionKey.OP_WRITE));
                        continue;
                    }

                    for (final Iterator<SelectionKey> i = selector.selectedKeys().iterator(); i.hasNext(); ) {
                        try {
                            final SelectionKey key = i.next();
                            final DatagramChannel datagramChannel = (DatagramChannel) key.channel();
                            final AttachmentClient attachmentClient = (AttachmentClient) key.attachment();

                            if (key.isReadable()) {
                                if (handleRead(datagramChannel, requests, attachmentClient)) {
                                    key.channel().close();
                                    continue;
                                } else {
                                    key.interestOps(SelectionKey.OP_WRITE);
                                }
                            }

                            if (key.isWritable()) {
                                handleWrite(datagramChannel, attachmentClient);
                                key.interestOps(SelectionKey.OP_READ);
                            }
                        } catch (IOException e) {
                            System.err.println("Some problems with connection");
                        } finally {
                            i.remove();
                        }
                    }
                }
            } finally {
                closeChannels(selector);
            }
        } catch (final IOException e) {
            e.printStackTrace();
        }
    }

    private void handleWrite(final DatagramChannel datagramChannel,
                             final AttachmentClient attachmentClient) throws IOException {

        putAndSend(datagramChannel,
                attachmentClient.getByteBuffer(),
                attachmentClient.getResponseData(),
                datagramChannel.getRemoteAddress());
    }

    private boolean handleRead(final DatagramChannel datagramChannel,
                               final int requests,
                               final AttachmentClient attachmentClient) throws IOException {

        receive(datagramChannel, attachmentClient.getByteBuffer());
        if (isValidMessage(bytesToString(attachmentClient.getByteBuffer()), attachmentClient.getThreadIndex(), attachmentClient.getRequestIndex())) {
            if (attachmentClient.getRequestIndex() < requests - 1) {
                attachmentClient.incrementRequestIndex();
            } else {
                return true;
            }
        }
        return false;
    }

    private void closeChannels(final Selector selector) {
        for (final SelectionKey key : selector.keys()) {
            if (key.channel() != null && key.channel().isOpen()) {
                try {
                    key.channel().close();
                } catch (IOException e) {
                    System.err.println("Can not close channel");
                }
            }
        }
    }

    private static class AttachmentClient {
        private final int threadIndex;
        private int requestIndex;
        private final ByteBuffer byteBuffer;
        private final String prefix;

        AttachmentClient(final int threadIndex, final int requestIndex, final int bufferSize, final String prefix) {
            this.threadIndex = threadIndex;
            this.requestIndex = requestIndex;
            this.byteBuffer = ByteBuffer.allocate(bufferSize);
            this.prefix = prefix;
        }

        protected ByteBuffer getByteBuffer() {
            return byteBuffer;
        }

        protected int getThreadIndex() {
            return threadIndex;
        }

        protected int getRequestIndex() {
            return requestIndex;
        }

        protected void incrementRequestIndex() {
            requestIndex++;
        }

        protected String getResponseMessage() {
            return String.format("%s%d_%d", prefix, threadIndex, requestIndex);
        }

        protected byte[] getResponseData() {
            return stringToBytes(getResponseMessage());
        }
    }

    public static void main(final String[] args) {
        mainClient(args, new HelloUDPNonblockingClient());
    }
}
