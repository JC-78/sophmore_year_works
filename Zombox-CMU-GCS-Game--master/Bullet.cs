using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Bullet : MonoBehaviour
{
    private Vector3 dir;
    private float maxDistance;
    private float travelledDistance = 0;
    public float moveSpeed = 80f;
    public float timeAlive = 5f;

    public void setup(Vector3 mouseVec)
    {
        this.dir = mouseVec.normalized;
        this.maxDistance = mouseVec.magnitude;
        transform.eulerAngles = new Vector3(0, 0, getAngleFromVector(dir)-90);
        Destroy(gameObject, timeAlive);
    }

    public void Update()
    {
        if (travelledDistance < maxDistance)
        {
            float d = moveSpeed * Time.deltaTime;
            travelledDistance += d;
            transform.position += dir * d;
        }
        else
        {
            Destroy(gameObject);
        }
        
    }

    private void OnTriggerEnter2D(Collider2D collision)
    {
        if (collision.gameObject.tag == "Bullet") Destroy(gameObject);
    }


    //return abs degrees angle from vector (float)
    public float getAngleFromVector(Vector3 dir)
    {
        float n = Mathf.Atan2(dir.y, dir.x) * Mathf.Rad2Deg;
        if (n < 0) n += 360;
        return n;
    }
}
