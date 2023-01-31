import java.util.concurrent.Semaphore;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import TSim.*;

public class Lab1Extra {

  public Lab1Extra(int speed1, int speed2) {
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
	
	 
	 static TrackMonitor intersectionMonitor = new TrackMonitor(0);
	 static TrackMonitor rightMonitor = new TrackMonitor(0);
	 static TrackMonitor middleMonitor = new TrackMonitor(0);
	 static TrackMonitor leftMonitor = new TrackMonitor(0);
	 static TrackMonitor upMonitor = new TrackMonitor(1);
	 static TrackMonitor lowMonitor = new TrackMonitor(1);
	 
	 
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
						rightMonitor.enter(this);
						tsi.setSwitch(17, 7, TSimInterface.SWITCH_RIGHT);
						
					}
					
					else if (isActive(15,8,event)) {
						rightMonitor.enter(this);
						tsi.setSwitch(17, 7, TSimInterface.SWITCH_LEFT);
					}
					
					else if (isActive(13,10,event) ){
						
						rightMonitor.leave();
					}
					else if (isActive(13,9,event)) {
						
						rightMonitor.leave();
					}
					///// Critical Up ////////////
					else if (isInactive(15,7,event)) {
						upMonitor.leave();
					}
					
					
					//////////////Critical Middle ///////////////////
					else if (isActive(16,9,event)) {
						
						if (middleMonitor.tryEnter()) {
							tsi.setSwitch(15,9, TSimInterface.SWITCH_RIGHT);
						}else {
							tsi.setSwitch(15,9, TSimInterface.SWITCH_LEFT);
						}
						
					}
					
					else if (isActive(3,9,event)) {
							
							middleMonitor.leave();
						
							
					}
					
					////////////////// critical left /////////////////////
					else if(isActive(6,10,event)) {
						leftMonitor.enter(this);
						tsi.setSwitch(4, 9, TSimInterface.SWITCH_RIGHT);
								
					}
					
					else if(isActive(6,9,event)) {
						leftMonitor.enter(this);
						tsi.setSwitch(4, 9, TSimInterface.SWITCH_LEFT);
						
					}
					
					
					else if (isActive(6,11,event)) {
						leftMonitor.leave();
					}
					else if (isActive(5,13,event)) {
						leftMonitor.leave();
					}
					
			////////////////////////////INTERSECTION //////////////////
					if(isActive(6,7,event)) {
						intersectionMonitor.enter(this);
					}
					
					else if(isActive(10,7,event)) {
						intersectionMonitor.leave();
					}
					
					else if(isActive(8,5,event)) {
						intersectionMonitor.enter(this);
					}
					
					else if(isActive(9,8,event)) {
						intersectionMonitor.leave();
					}
					else if (isActive(1,11,event)) {
						if(lowMonitor.tryEnter()) {
							
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
						rightMonitor.leave();
					}
					else if (isActive(15,8,event)) {
						rightMonitor.leave();
					}
					
					
					else if (isActive(13,9,event)) {
						rightMonitor.enter(this);
						tsi.setSwitch(15, 9, TSimInterface.SWITCH_RIGHT);
						
						
					}
					
					else if (isActive(13,10,event)) {
						rightMonitor.enter(this);
						tsi.setSwitch(15, 9, TSimInterface.SWITCH_LEFT);
					}
					/////////////// Critial Up ////////////
					else if (isActive(19,7,event)) {
						
						if(upMonitor.tryEnter()) {
							
							tsi.setSwitch(17, 7, TSimInterface.SWITCH_RIGHT);
						}else {
							tsi.setSwitch(17, 7, TSimInterface.SWITCH_LEFT);
						}
					}
			
					/////// critical middle /////////
					else if(isActive(3,9,event)) {
						if(middleMonitor.tryEnter()) {
							tsi.setSwitch(4,9, TSimInterface.SWITCH_LEFT);
						} else {
							tsi.setSwitch(4,9, TSimInterface.SWITCH_RIGHT);
						}
					}
					
					else if(isActive(16,9,event)) {
						middleMonitor.leave();
					}
					
					///////////// Critical Low //////////
					else if (isInactive(6,11,event)) {
						lowMonitor.leave();
						
					}
					/////// Critical left //////////////
					else if (isActive(6,11,event)) {
						leftMonitor.enter(this);
						tsi.setSwitch(3, 11, TSimInterface.SWITCH_LEFT);
					}
					
					else if (isActive(5,13,event)) {
						leftMonitor.enter(this);
						tsi.setSwitch(3, 11, TSimInterface.SWITCH_RIGHT);
					}
					

					else if (isActive(6,10,event)) {
						
						leftMonitor.leave();
					}
					else if (isActive(6,9,event)) {
						
						leftMonitor.leave();
					}
					
					///////////// Intersection //////////////////////
					else if(isActive(10,7,event)) {
						intersectionMonitor.enter(this);
						 
					}
					
					else if(isActive(6,7,event)) {
						intersectionMonitor.leave();
					}
					
					else if(isActive(8,5,event)) {
						intersectionMonitor.leave();
					}
					
					else if(isActive(9,8,event)) {
						intersectionMonitor.enter(this);
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
		public static class TrackMonitor{
			TSimInterface tsi = TSimInterface.getInstance();
			private Lock lock = new ReentrantLock();
			private int trainsOnTrack;
			private Condition emptyTrack = lock.newCondition();
			public TrackMonitor(int trainsOnTrack) {
				this.trainsOnTrack = trainsOnTrack;
			}
			
			public void enter(Train train) throws InterruptedException, CommandException {
				lock.lock();
				try {
					if (trainsOnTrack != 0) {
						tsi.setSpeed(train.trainId,0);
						emptyTrack.await();
					}
					trainsOnTrack ++;
					tsi.setSpeed(train.trainId,train.speed);
					
				}finally{
					lock.unlock();
				}
						
				
			}
			
			public void leave() {
				lock.lock();
				try {
					if (trainsOnTrack != 0) {
						trainsOnTrack--;
					}
					
					emptyTrack.signal();
				}finally {
					lock.unlock();
				}
		}
			public boolean tryEnter() {
				lock.lock();
				try {
					if (trainsOnTrack == 0) {
						trainsOnTrack++;
						return true;
					}else return false;
				}finally {
					lock.unlock();
				}			
			}
	}
}
