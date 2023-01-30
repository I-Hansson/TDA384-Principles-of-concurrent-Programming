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
	 static Semaphore semaMiddle = new Semaphore(1);
	 
	 static Semaphore semaUp = new Semaphore(0);
	 static Semaphore semaIntersection = new Semaphore(1);
	 static Semaphore semaLow = new Semaphore(0);
	 static Semaphore semaLeft = new Semaphore(1);
	 
	 
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

	 private boolean isActive(int xPos, int yPos, SensorEvent event) {
		 return (event.getXpos() == xPos && event.getYpos() == yPos && event.getStatus() == SensorEvent.ACTIVE);
	 }
	 private boolean isInactive(int xPos, int yPos, SensorEvent event) {
		 return (event.getXpos() == xPos && event.getYpos() == yPos && event.getStatus() == SensorEvent.INACTIVE);
	 }
	 
	public void run() {
		 while (true) {
			 try {
				SensorEvent event = tsi.getSensor(trainId);
				System.out.println("High " + semaUp.availablePermits());
				if (isActive(15,7,event) && this.direction == Direction.AtoB ) {
					tsi.setSpeed(trainId,0);
					semaRight.acquire();
					tsi.setSwitch(17, 7, TSimInterface.SWITCH_RIGHT);
					tsi.setSpeed(trainId, speed);
					
				}
				if (isInactive(15,7,event) && this.direction == Direction.AtoB ) {
					semaUp.release();
					
				}
				
				
				
				
				if (isActive(15,8,event) && this.direction == Direction.AtoB ) {
					tsi.setSpeed(trainId,0);
					semaRight.acquire();
					tsi.setSwitch(17, 7, TSimInterface.SWITCH_LEFT);
					tsi.setSpeed(trainId, speed);
					
					
				}
				if (isActive(15,7,event) && this.direction == Direction.BtoA) {
					semaRight.release();
				}
				if (isActive(15,8,event) && this.direction == Direction.BtoA) {
					semaRight.release();
				}
				//System.out.println(semaUp.availablePermits());
				if (isActive(19,7,event) && this.direction == Direction.BtoA) {
					
					if(semaUp.tryAcquire() == true) {
						
						tsi.setSwitch(17, 7, TSimInterface.SWITCH_RIGHT);
					}else {
						tsi.setSwitch(17, 7, TSimInterface.SWITCH_LEFT);
					}
				}
				
				
				if (isActive(13,10,event) && this.direction == Direction.AtoB ) {
					
					semaRight.release();
				}
				if (isActive(13,9,event) && this.direction == Direction.AtoB ) {
					
					semaRight.release();
				}
				
				if (isActive(13,9,event) && this.direction == Direction.BtoA) {
					System.out.println("stanna!");
					tsi.setSpeed(trainId,0);
					semaRight.acquire();
					tsi.setSwitch(15,9, TSimInterface.SWITCH_RIGHT);
					tsi.setSpeed(trainId, speed);
				}
				
				if (isActive(13,10,event) && this.direction == Direction.BtoA) {
					System.out.println("stanna!");
					tsi.setSpeed(trainId,0);
					semaRight.acquire();
					tsi.setSwitch(15,9, TSimInterface.SWITCH_LEFT);
					tsi.setSpeed(trainId, speed);
				}
				
				
			
				System.out.println(semaMiddle.availablePermits());
				if (isActive(16,9,event) && this.direction == Direction.AtoB) {
					if (semaMiddle.tryAcquire() == true) {
						tsi.setSwitch(15,9, TSimInterface.SWITCH_RIGHT);
					}else {
						tsi.setSwitch(15,9, TSimInterface.SWITCH_LEFT);
					}
				}
				
				if (isActive(3,9,event) && this.direction == Direction.AtoB) {
					if (semaMiddle.availablePermits() == 0 ){
						semaMiddle.release();
					}
						
				}
				
				if(isActive(3,9,event) && this.direction == Direction.BtoA ) {
					if(semaMiddle.tryAcquire() == true) {
						tsi.setSwitch(4,9, TSimInterface.SWITCH_LEFT);
					} else {
						tsi.setSwitch(4,9, TSimInterface.SWITCH_RIGHT);
					}
				}
				
				if(isActive(16,9,event) && this.direction == Direction.BtoA ) {
					if (semaMiddle.availablePermits() == 0 ){
						semaMiddle.release();
					}
				}
				
				if(isActive(6,10,event) && this.direction == Direction.AtoB ) {
					System.out.println("stanna!");
					tsi.setSpeed(trainId,0);
					semaLeft.acquire();
					tsi.setSwitch(4,9, TSimInterface.SWITCH_RIGHT);
					tsi.setSpeed(trainId, speed);
					
				}
				
				if(isActive(6,9,event) && this.direction == Direction.AtoB ) {
					System.out.println("stanna!");
					tsi.setSpeed(trainId,0);
					semaLeft.acquire();
					tsi.setSwitch(4,9, TSimInterface.SWITCH_LEFT);
					tsi.setSpeed(trainId, speed);
					
				}
				
			
				
				if (isActive(1,11,event) && this.direction == Direction.AtoB) {
					if(semaLow.tryAcquire() == true) {
						
						tsi.setSwitch(3, 11, TSimInterface.SWITCH_LEFT);
					}else {
						tsi.setSwitch(3, 11, TSimInterface.SWITCH_RIGHT);
					}
				}
				if (isInactive(6,11,event) && this.direction == Direction.BtoA ) {
					semaLow.release();
					
				}
				
				if (isActive(6,11,event) && this.direction == Direction.AtoB) {
					semaLeft.release();
				}
				if (isActive(5,13,event) && this.direction == Direction.AtoB) {
					semaLeft.release();
				}
				
				
				System.out.println("left" + semaLeft.availablePermits() );
				if (isActive(6,11,event) && this.direction == Direction.BtoA ) {
					tsi.setSpeed(trainId,0);
					semaLeft.acquire();
					tsi.setSwitch(3, 11, TSimInterface.SWITCH_LEFT);
					tsi.setSpeed(trainId, speed);
					
				}
				
				if (isActive(5,13,event) && this.direction == Direction.BtoA ) {
					tsi.setSpeed(trainId,0);
					semaLeft.acquire();
					tsi.setSwitch(3, 11, TSimInterface.SWITCH_RIGHT);
					tsi.setSpeed(trainId, speed);
					
				}
				
				
				

				if (isActive(6,10,event) && this.direction == Direction.BtoA ) {
					
					semaLeft.release();
				}
				if (isActive(6,9,event) && this.direction == Direction.BtoA ) {
					
					semaLeft.release();
				}
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				////////////////////////////INTERSECTION //////////////////
				if(isActive(6,7,event) && this.direction == Direction.AtoB ) {
					tsi.setSpeed(trainId,0);
					semaIntersection.acquire();
					tsi.setSpeed(trainId,speed);
				}
				
				if(isActive(10,7,event) && this.direction == Direction.AtoB ) {
					semaIntersection.release();
				}
				
				if(isActive(8,5,event) && this.direction == Direction.AtoB ) {
					tsi.setSpeed(trainId,0);
					semaIntersection.acquire();
					tsi.setSpeed(trainId,speed);
				}
				
				if(isActive(9,8,event) && this.direction == Direction.AtoB ) {
					semaIntersection.release();
				}
				
				////////////////////////////////////////////////////////////
				if(isActive(10,7,event) && this.direction == Direction.BtoA ) {
					tsi.setSpeed(trainId,0);
					semaIntersection.acquire();
					tsi.setSpeed(trainId,speed);
					
				}
				
				if(isActive(6,7,event) && this.direction == Direction.BtoA ) {
					semaIntersection.release();
				}
				
				if(isActive(8,5,event) && this.direction == Direction.BtoA ) {
					semaIntersection.release();
				}
				
				if(isActive(9,8,event) && this.direction == Direction.BtoA ) {
					tsi.setSpeed(trainId,0);
					semaIntersection.acquire();
					tsi.setSpeed(trainId,speed);
				}
				
				
	
				/////////////////////////////AT STATION /////////////////
				
				if(isActive(15,11,event) && this.direction == Direction.AtoB ) {
					tsi.setSpeed(trainId,0);
					Thread.sleep(1000 + (20 * Math.abs(this.speed)));
					flipDir();
					this.speed *= -1;
					tsi.setSpeed(trainId,speed);
					
				}
				if(isActive(15,13,event) && this.direction == Direction.AtoB ) {
					tsi.setSpeed(trainId,0);
					Thread.sleep(1000 + (20 * Math.abs(this.speed)));
					flipDir();
					this.speed *= -1;
					tsi.setSpeed(trainId,speed);
					
				}
				if(isActive(15,3,event) && this.direction == Direction.BtoA ) {
					tsi.setSpeed(trainId,0);
					Thread.sleep(1000 + (20 * Math.abs(this.speed)));
					flipDir();
					this.speed *= -1;
					tsi.setSpeed(trainId,speed);
					
				}
				
				if(isActive(15,5,event) && this.direction == Direction.BtoA ) {
					tsi.setSpeed(trainId,0);
					Thread.sleep(1000 + (20 * Math.abs(this.speed)));
					flipDir();
					this.speed *= -1;
					tsi.setSpeed(trainId,speed);
					
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
