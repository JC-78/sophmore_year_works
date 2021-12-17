using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class PlayerController : MonoBehaviour
{

    [SerializeField] private Transform pfBullet;

    // Start is called before the first frame update
    void Start()
    {
      
    }

    public float spd = 5f;

    // Update is called once per frame
    void Update()
    {
        transform.position += (new Vector3(Input.GetAxis("Horizontal"), Input.GetAxis("Vertical"), 0)) * spd * Time.deltaTime;

        if (Input.GetMouseButtonDown(0))
            this.Shoot();
    }

    void Shoot()
    {
        Vector3 shootDir = (Camera.main.ScreenToWorldPoint(Input.mousePosition) - transform.position).normalized;

        Transform bulletTransform = Instantiate(pfBullet, transform.position, Quaternion.identity);
        bulletTransform.GetComponent<Bullet>().setup(shootDir);
    }
}
