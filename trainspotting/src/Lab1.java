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


  private class Train implements Runnable {
    private final int id;
    TSimInterface tsi = TSimInterface.getInstance();
    int[] rMiddleSensor = new int[]{16, 9};
    int[] rlMiddleSensor = new int[]{15, 10};
    int[] rMiddleSwitch = new int[]{15, 9};
    int[] lMiddleSensor = new int[]{3, 9};
    int[] llMiddleSensor = new int[]{4, 10};
    int[] lMiddleSwitch = new int[]{4, 9};
    Section[] sections = new Section[]{new Section(15, 9, 16, 9, 15, 10)};
    static Semaphore middleSemaphore = new Semaphore(1);

    Train(int id, int speed) throws CommandException{
      this.id = id;
      init(speed);
    }

    public void run() {
      try {
        TwoSensorSection section = null;
        while (true) {
          SensorEvent event = tsi.getSensor(id);
          
          for (TwoSensorSection s : sections) {
            if (s.thisSection(event.getXpos(), event.getYpos())) {
              section = s;
            }
          }

          section.toggleSection();
          /* 
          if (event.getXpos() == rMiddleSensor[0] && event.getStatus() == SensorEvent.ACTIVE) {
            if (middleSemaphore.availablePermits() == 0) {
              tsi.setSwitch(rMiddleSwitch[0], rMiddleSwitch[1], tsi.SWITCH_LEFT); 
              while (true) {
                event = tsi.getSensor(id);
                if (event.getXpos() == rlMiddleSensor[0] && event.getStatus() == SensorEvent.INACTIVE) {
                  tsi.setSwitch(rMiddleSwitch[0], rMiddleSwitch[1], tsi.SWITCH_RIGHT);
                }
                if (event.getXpos() == llMiddleSensor[0] && event.getStatus() == SensorEvent.ACTIVE) {
                  tsi.setSwitch(lMiddleSwitch[0], lMiddleSwitch[1], tsi.SWITCH_RIGHT);
                }
              }
            
            } else {
              middleSemaphore.acquire();
              while (true) {
                event = tsi.getSensor(id);
                if (event.getXpos() == lMiddleSensor[0] && event.getStatus() == SensorEvent.ACTIVE) {
                  middleSemaphore.release();
                }
              }
            }
          }

          else if (event.getXpos() == lMiddleSensor[0] && event.getStatus() == SensorEvent.ACTIVE) {
            if (middleSemaphore.availablePermits() == 0) {
              tsi.setSwitch(lMiddleSwitch[0], lMiddleSwitch[1], tsi.SWITCH_RIGHT);
              while (true) {
                event = tsi.getSensor(id);
                if (event.getXpos() == rlMiddleSensor[0] && event.getStatus() == SensorEvent.ACTIVE) {
                  tsi.setSwitch(rMiddleSwitch[0], rMiddleSwitch[1], tsi.SWITCH_RIGHT);
                }
                if (event.getXpos() == llMiddleSensor[0] && event.getStatus() == SensorEvent.INACTIVE) {
                  tsi.setSwitch(lMiddleSwitch[0], lMiddleSwitch[1], tsi.SWITCH_LEFT);
                }
              }
            } else {
              middleSemaphore.acquire();
              while (true) {
                event = tsi.getSensor(id);
                if (event.getXpos() == rMiddleSensor[0] && event.getStatus() == SensorEvent.ACTIVE) {
                  middleSemaphore.release();
                }
              }
            }
          }*/

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
  
  private class TwoSensorSection{
      private int switchX;
      private int switchY;
      private int sensor1X;
      private int sensor1Y;
      private int sensor2X;
      private int sensor2Y;
      private TSimInterface tsi = TSimInterface.getInstance();
      private int trainId;
      private Semaphore semaphore;

      public TwoSensorSection(int trainId, int[] switch1, int[] sensor1, int[] sensor2, Semaphore semaphore) {
        this.switchX = switch1[0];
        this.switchY = switch1[1];
        this.sensor1X = sensor1[0];
        this.sensor1Y = sensor1[1];
        this.sensor2X = sensor2[0];
        this.sensor2Y = sensor2[1];
        this.semaphore = semaphore;
      }

      private boolean thisSection(int x, int y) {
        return (sensor1X == x && sensor1Y == y) || (sensor2X == x && sensor2Y == y);
      }

      private void toggleSection (int x, int y) throws CommandException, InterruptedException {
        if (semaphore.tryAcquire()) {

        } else if (sensor1X == x && sensor1Y == y) {
          makeSwitch();
        } else if (sensor2X == x && sensor2Y == y) {
          reverseSwitch();
        }
      }

      private boolean tryAcquire() {
        return semaphore.tryAcquire();
      }

      private void reverseSwitch() throws CommandException, InterruptedException {
        tsi.setSwitch(switchX, switchY, tsi.SWITCH_LEFT); 
        while (true) {
          SensorEvent event = tsi.getSensor(trainId);
          if (event.getXpos() == sensor1X && event.getStatus() == SensorEvent.ACTIVE) {
            tsi.setSwitch(switchX, switchY, tsi.SWITCH_RIGHT);
          }
        }
      }

      private void makeSwitch() throws CommandException, InterruptedException {
        tsi.setSwitch(switchX, switchY, tsi.SWITCH_LEFT); 
        while (true) {
          SensorEvent event = tsi.getSensor(trainId);
          if (event.getXpos() == sensor2X && event.getStatus() == SensorEvent.INACTIVE) {
            tsi.setSwitch(switchX, switchY, tsi.SWITCH_RIGHT);
          }
        }
      }
    }
    /* 
    private class FourSensorSection extends TwoSensorSection{
      private int sensor3X;
      private int sensor3Y;
      private int sensor4X;
      private int sensor4Y;
      private int switch2X;
      private int switch2Y;

      public FourSensorSection(int trainId, int[] switch1, int[] switch2, int[] sensor1, int[] sensor2, int[] sensor3, int[] sensor4) {
        super(trainId, switch1, sensor1, sensor2);
        this.sensor3X = sensor3[0];
        this.sensor3Y = sensor3[1];
        this.sensor4X = sensor4[0];
        this.sensor4Y = sensor4[1];
        this.switch2X = switch2[0];
        this.switch2Y = switch2[1];
      }

      private void makeSwitch() throws CommandException, InterruptedException {

        tsi.setSwitch(switchX, switchY, tsi.SWITCH_LEFT); 
        while (true) {
          SensorEvent event = tsi.getSensor(trainId);
          if (event.getXpos() == sensor2X && event.getStatus() == SensorEvent.INACTIVE) {
            tsi.setSwitch(switchX, switchY, tsi.SWITCH_RIGHT);
          }
        }
      }
      

    }
    */
 
}
