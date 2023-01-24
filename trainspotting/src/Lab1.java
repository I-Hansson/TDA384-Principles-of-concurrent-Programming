import java.util.concurrent.Semaphore;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import TSim.*;

public class Lab1 {
  

  public Lab1(int speed1, int speed2) {
    TSimInterface tsi = TSimInterface.getInstance();
    int[] rMiddleSensor = new int[]{18, 9};
    int[] lMiddleSensor = new int[]{2, 9};
    

    try {
      init(tsi);

      Thread t1 = new Thread(new Train(1, speed1));
      t1.start();
      Thread t2 = new Thread(new Train(2, speed2));
      t2.start();
    }
    catch (CommandException e) {
      e.printStackTrace();    // or only e.getMessage() for the error
      System.exit(1);
    }
  }

  private void init(TSimInterface tsi) throws CommandException{
    tsi.setSwitch(15, 9, tsi.SWITCH_RIGHT);
    tsi.setSwitch(17, 7, tsi.SWITCH_RIGHT);
  }

  private class Sensor {
    private final int xCoord;
    private final int yCoord;

    public Sensor(int xCoord, int yCoord) {
      this.xCoord = xCoord;
      this.yCoord = yCoord;
    }

    public int[] getCoords() {
      return new int[]{xCoord, yCoord};
    }
  }

  private class Train implements Runnable{
    private final int id;
    TSimInterface tsi = TSimInterface.getInstance();
    int[] rMiddleSensor = new int[]{16, 9};
    int[] rlMiddleSensor = new int[]{15, 10};
    int[] rMiddleSwitch = new int[]{15, 9};
    int[] lMiddleSensor = new int[]{3, 9};
    int[] llMiddleSensor = new int[]{4, 10};
    int[] lMiddleSwitch = new int[]{4, 9};
    static Semaphore middleSemaphore = new Semaphore(1);

    Train(int id, int speed) throws CommandException{
      this.id = id;
      init(speed);
    }

    public void run() {
      try {
        while (true) {
          SensorEvent event = tsi.getSensor(id);

          if (event.getXpos() == rMiddleSensor[0] && event.getStatus() == SensorEvent.ACTIVE) {
            if (middleSemaphore.availablePermits() == 0) {
              tsi.setSwitch(rMiddleSwitch[0], rMiddleSwitch[1], tsi.SWITCH_LEFT);
              tsi.setSwitch(lMiddleSwitch[0], lMiddleSwitch[1], tsi.SWITCH_RIGHT);
            } else {
              middleSemaphore.acquire();
            }
          }

          else if (event.getXpos() == lMiddleSensor[0] && event.getStatus() == SensorEvent.ACTIVE) {
            if (middleSemaphore.availablePermits() == 0) {
              tsi.setSwitch(lMiddleSwitch[0], lMiddleSwitch[1], tsi.SWITCH_RIGHT);
              tsi.setSwitch(rMiddleSwitch[0], rMiddleSwitch[1], tsi.SWITCH_LEFT);
            } else {
              middleSemaphore.acquire();
            }
          }

        }
      } catch (CommandException e) {
        e.printStackTrace();    // or only e.getMessage() for the error
        System.exit(1);
      } catch (Exception e) {
        e.printStackTrace();    // or only e.getMessage() for the error
        System.exit(1);
      }
    }

    private void init(int speed) throws CommandException{
        tsi.setSpeed(id, speed);
    }
  }
 
}
