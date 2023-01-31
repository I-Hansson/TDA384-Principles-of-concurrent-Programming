import java.util.concurrent.Semaphore;

import TSim.*;

public class Lab1 {

  public Lab1(int speed1, int speed2) {
    TSimInterface tsi = TSimInterface.getInstance();
    
    try {
    	
    	
    	Thread train1 = new Thread(new Train(1,speed1));
  
    	Thread train2 = new Thread(new Train(2,speed2));
    	train1.start();
    	train2.start();
      
    }
    catch (CommandException e) {
      e.printStackTrace();    // or only e.getMessage() for the error
      System.exit(1);
    }
    
    
    
  }

  
  	static class Train implements Runnable{
	 
	 protected int trainId;
	 protected int speed;
	 protected Direction direction;
	 static Semaphore semaRight = new Semaphore(1);
	 static Semaphore semaLeft = new Semaphore(1);
	 static Semaphore semaMiddle = new Semaphore(1);
	 static Semaphore semaIntersection = new Semaphore(1);
	 static Semaphore semaUp = new Semaphore(0);
	 static Semaphore semaLow = new Semaphore(0);
	 
	 TSimInterface tsi = TSimInterface.getInstance();
	 protected Train(int trainId, int speed) throws CommandException{
		 
		 this.trainId = trainId;
		 this.speed = speed;
		 tsi.setSpeed(trainId,speed);
		 initDir();
		 
	 }

	 public enum Direction{
		 AtoB,
		 BtoA
	 }
	 private void initDir() {
		 if(trainId == 1) {
			 this.direction = Direction.AtoB;
		 }else {
			 this.direction = Direction.BtoA;
		 }
	
		
	}
	 private void flipDir() {
		 if (this.direction == Direction.AtoB) {
			 this.direction = Direction.BtoA;
		 }else {
			 this.direction = Direction.AtoB;
		 }

	 }
	 /**
	  * Checks if a sensor is active
	  * @param xPos
	  * @param yPos
	  * @param event
	  * @return returns True if the given sensor is active
	  */
	 private boolean isActive(int xPos, int yPos, SensorEvent event) {
		 return (event.getXpos() == xPos && event.getYpos() == yPos && event.getStatus() == SensorEvent.ACTIVE);
	 }
	 /**
	  * Checks if a sensor is inactive
	  * @param xPos
	  * @param yPos
	  * @param event
	  * @return returns True if the given sensor is inactive
	  */
	 private boolean isInactive(int xPos, int yPos, SensorEvent event) {
		 return (event.getXpos() == xPos && event.getYpos() == yPos && event.getStatus() == SensorEvent.INACTIVE);
	 }
	 /**
	  * Wait at switch until critical section is available
	  * @param x
	  * @param y
	  * @param switchDir
	  * @param semaphore
	  * @throws CommandException
	  * @throws InterruptedException
	  */
	 private void waitAtSwitch(int x, int y, int switchDir, Semaphore semaphore) throws CommandException, InterruptedException {
		 tsi.setSpeed(trainId, 0);
		 semaphore.acquire();     // Thread sleeps if no permit available. 
		 tsi.setSwitch(x, y, switchDir);
		 tsi.setSpeed(trainId,speed);
		 
	 }
	 /**
	  * When train at a station, wait for a second and flip direction and speed.
	  * @throws InterruptedException
	  * @throws CommandException
	  */
	 private void atStation() throws InterruptedException, CommandException {
		 tsi.setSpeed(trainId,0);
		 Thread.sleep(1000 + (20 * Math.abs(this.speed)));
		 flipDir();
		 this.speed *= -1;
		 tsi.setSpeed(trainId,speed);
	 }
	 
	public void run() {
		 while (true) {
			 try {
				SensorEvent event = tsi.getSensor(trainId);
				
				if ( this.direction == Direction.AtoB) {
					/// Critical right //////////
					if (isActive(15,7,event)) {
						waitAtSwitch(17,7,TSimInterface.SWITCH_RIGHT,semaRight);	
					}
					
					else if (isActive(15,8,event)) {
						waitAtSwitch(17,7,TSimInterface.SWITCH_LEFT,semaRight);
					}
					
					else if (isActive(13,10,event) ){
						
						semaRight.release();
					}
					else if (isActive(13,9,event)) {
						
						semaRight.release();
					}
					///// Critical Up ////////////
					else if (isInactive(15,7,event)) {
						semaUp.release();
					}
					
					
					//////////////Critical Middle ///////////////////
					else if (isActive(16,9,event)) {
						if (semaMiddle.tryAcquire() == true) {
							tsi.setSwitch(15,9, TSimInterface.SWITCH_RIGHT);
						}else {
							tsi.setSwitch(15,9, TSimInterface.SWITCH_LEFT);
						}
					}
					
					else if (isActive(3,9,event)) {
						if (semaMiddle.availablePermits() == 0 ){
							semaMiddle.release();
						}
							
					}
					
					////////////////// critical left /////////////////////
					else if(isActive(6,10,event)) {
						waitAtSwitch(4,9,TSimInterface.SWITCH_RIGHT,semaLeft);				
					}
					
					else if(isActive(6,9,event)) {
						waitAtSwitch(4,9,TSimInterface.SWITCH_LEFT,semaLeft);
					}
					
					
					else if (isActive(6,11,event)) {
						semaLeft.release();
					}
					else if (isActive(5,13,event)) {
						semaLeft.release();
					}
					
			////////////////////////////INTERSECTION //////////////////
					if(isActive(6,7,event)) {
						tsi.setSpeed(trainId,0);
						semaIntersection.acquire();
						tsi.setSpeed(trainId,speed);
					}
					
					else if(isActive(10,7,event)) {
						semaIntersection.release();
					}
					
					else if(isActive(8,5,event)) {
						tsi.setSpeed(trainId,0);
						semaIntersection.acquire();
						tsi.setSpeed(trainId,speed);
					}
					
					else if(isActive(9,8,event)) {
						semaIntersection.release();
					}
					else if (isActive(1,11,event)) {
						if(semaLow.tryAcquire()) {
							
							tsi.setSwitch(3, 11, TSimInterface.SWITCH_LEFT);
						}else {
							tsi.setSwitch(3, 11, TSimInterface.SWITCH_RIGHT);
						}
					}
					//////////////station /////////////////
					else if(isActive(15,11,event)) {
						atStation();
					}
					else if(isActive(15,13,event)) {
						atStation();
						
					}
					
				}else {
					/////////////// Critical right
					if (isActive(15,7,event)) {
						semaRight.release();
					}
					else if (isActive(15,8,event)) {
						semaRight.release();
					}
					
					
					else if (isActive(13,9,event)) {
						waitAtSwitch(15,9,TSimInterface.SWITCH_RIGHT,semaRight);
						
					}
					
					else if (isActive(13,10,event)) {
						waitAtSwitch(15,9,TSimInterface.SWITCH_LEFT,semaRight);
					}
					/////////////// Critial Up ////////////
					else if (isActive(19,7,event)) {
						
						if(semaUp.tryAcquire()) {
							
							tsi.setSwitch(17, 7, TSimInterface.SWITCH_RIGHT);
						}else {
							tsi.setSwitch(17, 7, TSimInterface.SWITCH_LEFT);
						}
					}
			
					/////// critical middle /////////
					else if(isActive(3,9,event)) {
						if(semaMiddle.tryAcquire() == true) {
							tsi.setSwitch(4,9, TSimInterface.SWITCH_LEFT);
						} else {
							tsi.setSwitch(4,9, TSimInterface.SWITCH_RIGHT);
						}
					}
					
					else if(isActive(16,9,event)) {
						if (semaMiddle.availablePermits() == 0 ){
							semaMiddle.release();
						}
					}
					
					///////////// Critical Low //////////
					else if (isInactive(6,11,event)) {
						semaLow.release();
						
					}
					/////// Critical left //////////////
					else if (isActive(6,11,event)) {
						waitAtSwitch(3,11,TSimInterface.SWITCH_LEFT,semaLeft);
					}
					
					else if (isActive(5,13,event)) {
						waitAtSwitch(3,11,TSimInterface.SWITCH_RIGHT,semaLeft);
					}
					

					else if (isActive(6,10,event)) {
						
						semaLeft.release();
					}
					else if (isActive(6,9,event)) {
						
						semaLeft.release();
					}
					
					///////////// Intersection //////////////////////
					else if(isActive(10,7,event)) {
						tsi.setSpeed(trainId,0);
						semaIntersection.acquire();
						tsi.setSpeed(trainId,speed);
						
					}
					
					else if(isActive(6,7,event)) {
						semaIntersection.release();
					}
					
					else if(isActive(8,5,event)) {
						semaIntersection.release();
					}
					
					else if(isActive(9,8,event)) {
						tsi.setSpeed(trainId,0);
						semaIntersection.acquire();
						tsi.setSpeed(trainId,speed);
					}

					/////////////////////////////AT STATION /////////////////

					else if(isActive(15,3,event)) {
						atStation();
						
					}
					
					else if(isActive(15,5,event)) {
						atStation();
						
					}
					
				}

			} catch (CommandException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		 }
		 
	 }
	 
  	}

}
